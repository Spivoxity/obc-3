/*
 * gc.c
 * 
 * This file is part of the Oxford Oberon-2 compiler
 * Copyright (c) 2006--2016 J. M. Spivey
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "obx.h"

/* Define MULTIBLOCKS to allow splitting of multi-page blocks */
#undef MULTIBLOCKS

static mybool debug[256];	/* Debugging flags */
/* a - print addresses; g - print [GC...];
   b - print chunks allocated; c - print every allocation;
   d - general debugging; l - trace low-level allocator; m - print maps;  
   z - GC on each allocation */

/* Assertions are enabled in all programs if DEBUG is defined */
#ifdef DEBUG
static const char *assert_fmt = "*assertion %s failed on line %d of file %s";
#define ASSERT(p) \
     if (! (p)) panic(assert_fmt, #p, __LINE__, __FILE__);
#else
#define ASSERT(p)
#endif

/* Debugging messages are present only in 'obtrace'. */
#ifdef TRACE
#define DEBUG_PRINT(flag, args) \
     if (debug[flag]) { printf args; fflush(stdout); }
#else
#define DEBUG_PRINT(flags, args)
#endif

/* [GC] message is always present. */
#define GC_TRACE(s) if (debug['g']) { printf("%s", s); fflush(stdout); }

/* There are three storage allocators: the lower one that deals in
   whole pages, the upper one that gets pages from the lower one and
   splits them into smaller objects, and a separate scratch allocator
   that is used for internal data structures of the heap, and for the
   program's symbol table.  Objects allocated in the scratch space are
   not garbage collected, but they don't need to follow the
   collector's layout rules.  All this can coexist with malloc(),
   which may well be used by stdio to allocate buffers. */

/* Terminology: a PAGE has a fixed size, determined by configure.  A
   BLOCK is a contiguous area of one or more pages.  An OBJECT is a
   memory area allocated for a client.  For the allocator to function
   correctly, the function get_memory must be able to allocate memory
   in chunks of size CHUNK_SIZE aligned on a PAGESIZE boundary: this
   more-or-less implies that CHUNK_SIZE is a multiple of the VM page
   size, and the VM page size is a multiple of PAGESIZE.  Configure
   satisfies this by making PAGESIZE equal to the size of a VM page.

   For each small size of object, there is a POOL of blocks that are
   split up by the upper-level allocator into objects of that size.
   Large objects occupy an entire block of one or more pages.
   We use compaction in each pool of small objects; large objects are
   not compacted, and never move. */

#define BYTES_PER_WORD 4
#define PAGE_WORDS (PAGESIZE / BYTES_PER_WORD)

#define MB 1024*1024
#define INIT_SIZE (2*MB) /* Initial heap size */
#define CHUNK_SIZE (1*MB) /* Amount that heap grows */

#define round_down(x, n) ((x)/(n)*(n))
#define round_up(x, n)   round_down((x)+(n)-1, n)

typedef unsigned word;

/* Most of the manipulations here are done in terms of words, and to
   save brain cells, we assume a word has 32 bits; there are lots of
   constants that need changing if that is not true.  We also assume
   that a page index occupies exactly one page (see below). */

#ifdef HAVE_MMAP
#include <fcntl.h>
#include <sys/mman.h>

#ifdef M64X32
#define mmap_flags MAP_PRIVATE|MAP_32BIT
#else
#define mmap_flags MAP_PRIVATE
#endif

#ifdef MACOS
#define MAP_ANONYMOUS MAP_ANON
#endif

static void *grab_chunk(unsigned size) {
     void *p;
     static void *last_addr = NULL;

#ifdef MAP_ANONYMOUS
     p = mmap(last_addr, size, PROT_READ|PROT_WRITE, 
	      mmap_flags|MAP_ANONYMOUS, -1, 0);
#else
     static int zero_fd = -1;

     if (zero_fd < 0) {
	  zero_fd = open("/dev/zero", O_RDONLY);
	  if (zero_fd < 0) panic("couldn't open /dev/zero");
     }

     p = mmap(last_addr, size, PROT_READ|PROT_WRITE,
              mmap_flags, zero_fd, 0);
#endif

     if (p == MAP_FAILED) return NULL;
     last_addr = p;
     return p;
}
#endif

#ifdef WINDOWS
#include <windows.h>

static void *grab_chunk(unsigned size) {
     return VirtualAlloc(NULL, size, MEM_COMMIT|MEM_RESERVE,
			 PAGE_EXECUTE_READWRITE);
}
#endif

/* get_memory -- grab one or more pages from the operating system */
static void *get_memory(unsigned size) {
     unsigned alloc_size = round_up(size, PAGESIZE);
     void *p;

     /* This happens e.g. if custom translation makes the code size zero */
     if (alloc_size == 0) return NULL;

     DEBUG_PRINT('b', ("Need %u; requesting chunk of size %u\n",
                       size, alloc_size));
     p = grab_chunk(alloc_size);
     if (p == NULL) panic("out of memory");
     DEBUG_PRINT('b', ("Allocated chunk at %p\n", p));
     ASSERT((ptrtype) p % PAGESIZE == 0);
     return p;
}


/* SCRATCH ALLOCATOR */

/* Scratch storage is managed quite separately from the heap, since
   combining the two would gain little efficiency at the expense of a
   lot of complexity.  We allocate whole pages (e.g. for the page
   table) on page boundaries. Scratch blocks must be aligned on an
   8-byte boundary for architectures that don't support unaligned
   loads and stores of long long unsigned, a type that is used for
   profiling counts. */

#define SCRATCH_ALIGN 8

/* In order to manage a 4MB heap, we need about 1024 headers and 2 or
   3 page indexes, making about 8 pages of scratch storage.  We also
   need space for the program's symbol table.  Grabbing scratch space
   16 pages at a time seems a fair compromise. */

#define SCRATCH_CHUNK (16 * PAGESIZE)

/* The scratch allocator keeps hold of just one piece of free memory,
   and wastefully discards it if it is too small to satisfy the next
   memory request. */

static uchar *scratch_free = NULL;
static uchar *scratch_limit = NULL;

void *scratch_alloc(unsigned size) {
     unsigned alloc_size = round_up(size, SCRATCH_ALIGN);
     uchar *p;

     if (scratch_free == NULL || alloc_size > scratch_limit - scratch_free) {
	  if (alloc_size > SCRATCH_CHUNK
	      || (scratch_free != NULL
                  && scratch_limit - scratch_free >= 4*PAGESIZE))
	       /* Avoid discarding a largish piece */
	       return get_memory(alloc_size);

	  scratch_free = (uchar *) get_memory(SCRATCH_CHUNK);
	  scratch_limit = scratch_free + SCRATCH_CHUNK;
     }

     if (alloc_size % PAGESIZE == 0) {
	  scratch_limit -= alloc_size;
	  p = scratch_limit;
     } else {
	  p = scratch_free;
	  scratch_free += alloc_size;
     }

     ASSERT((ptrtype) p % SCRATCH_ALIGN == 0);

     return p;
}


/* BLOCK HEADERS */

/* Each heap block has a header, separate from the block itself, that
   is allocated in scratch space.  A heap block contains only one size
   of object, given by the h_objsize field; this makes it possible to
   find the start of an object given a pointer to its interior.  Also,
   heap blocks are given a timestamp that allows us to identify during
   GC which semispace they belong to. */

typedef struct _header {
     uchar *h_memory;		/* The block itself */
     unsigned h_size;		/* Size of block (bytes) */
     unsigned h_objsize;	/* Size of each object (bytes), or 0 if free */
     unsigned h_epoch;		/* Timestamp to identify semispace */
     struct _header *h_next, *h_prev; /* Adjacent blocks in some list */
} header;

/* Headers can become free when two blocks merge into one, so we keep
   a free list for them and allocate from it when possible */

static header *hdr_free = NULL;

/* alloc_header -- create a block header */
static header *alloc_header(void) {
     header *h;

     if (hdr_free == NULL) 
	  h = (header *) scratch_alloc(sizeof(header));
     else {
	  h = hdr_free;
	  hdr_free = h->h_next;
     }

     h->h_memory = NULL;
     h->h_size = 0;
     h->h_objsize = 0;
     h->h_epoch = 0;
     h->h_next = h->h_prev = NULL;
     return h;
}

#define free_header(h) h->h_next = hdr_free; hdr_free = h;

/* Each block is linked into one of several doubly-linked lists: there
   are lists of free blocks of various sizes, lists of blocks that are
   in use for allocating various small sizes of object, and a list of
   blocks in use for big objects.  All these lists are given a
   dummy node to simplify pointer manipulations. */

static header *new_list(void) {
     header *h = alloc_header();
     h->h_next = h->h_prev = h;
     return h;
}

#define empty(list) ((list)->h_next == (list))

#define insert(hdr, h2)					\
     h2->h_next = hdr; h2->h_prev = hdr->h_prev;	\
     hdr->h_prev->h_next = h2; hdr->h_prev = h2;

#define unlink(h) \
     h->h_prev->h_next = h->h_next; h->h_next->h_prev = h->h_prev

/* Say "for (headers(h, list))" to traverse a cyclic list of headers. */
#define headers(h, list) \
     header *h = list->h_next; h != list; h = h->h_next


/* PAGE TABLE */

/* We must deal with interior pointers, so we need to find the start
   of any object, given an address anywhere within it.  (Unlike C, we
   don't need to deal with addresses that are just off the end.)  To
   this end, we keep a kind of page table covering the whole address
   space, organising it as a two-level tree (an idea from the Boehm
   collector).  For large blocks we make several entries in the table
   point to the same block header.  All storage, both allocated and
   free, that belongs to the heap is mapped in the page table.  The
   page table also makes it easy to find the neighbours of any
   block. */

/* To use the two-level table, we need to split an address into three
   parts: the top part (10 bits for PAGESIZE = 4096), which selects
   an index; the bottom part (10 bits), which selects a page under
   that index, and the offset (12 bits) within the page.  In general,
   we arrange that a page index occupies one page itself, and
   calculate the size of the root table to cover the address space. */

#define BOT_BITS (LOG_PAGESIZE - 2)
#define BOT_SIZE (1 << BOT_BITS)
#define TOP_BITS (8*BYTES_PER_WORD - BOT_BITS - LOG_PAGESIZE)
#define TOP_SIZE (1 << TOP_BITS)

#define mask(x, n) ((x) & ((1 << (n)) - 1))

#define top_part(p)  (address(p) >> (BOT_BITS + LOG_PAGESIZE))
#define bot_part(p)  mask(address(p) >> LOG_PAGESIZE, BOT_BITS)

/* Here's the layout of the page table; unused elements of the
   top-level table are all initialized to empty_index, a page full
   of NULLs. */

typedef word page_index[BOT_SIZE];

static word page_table[TOP_SIZE];
static page_index *empty_index;

#define get_header(p) \
     ptrcast(header, (*ptrcast(page_index, \
                               page_table[top_part(p)]))[bot_part(p)])

/* page_setup -- make page table entries point to a given header */
static void page_setup(uchar *base, unsigned size, header *h) {
     ASSERT(size % PAGESIZE == 0);

     for (uchar *p = base; p < base + size; p += PAGESIZE) {
	  /* Make sure lower index exists */
	  if (page_table[top_part(p)] == address(empty_index))
	       page_table[top_part(p)] = 
		    address(scratch_alloc(sizeof(page_index)));

          (*ptrcast(page_index, page_table[top_part(p)]))[bot_part(p)]
               = address(h);
     }
}

/* To assist in merging free blocks, we can find the two blocks that
   surround a given block */
#define left_neighbour(h) get_header(h->h_memory - 1)
#define right_neighbour(h) get_header(h->h_memory + h->h_size)


/* LOWER-LEVEL ALLOCATOR */

/* We maintain BIG_BLOCK free lists for free blocks of size 1, 2,
   ... BIG_BLOCK-1 pages, and a last free list for those with size >=
   BIG_BLOCK pages.  Free blocks are merged with their neighbours, and
   all storage on the free lists is zeroed.  (free_list[0] is never
   used.) 

   The purpose here is to reduce fragmentation by using small blocks
   when possible.  Since a very common case is allocating a single
   page when only a few big blocks are free, we should keep BIG_BLOCK
   fairly small, however. */

#define BIG_BLOCK 8

static header *free_list[BIG_BLOCK+1];
static unsigned gencount = 1;	    /* Timestamp */

/* make_free -- add a block to the appropriate free list */
static void make_free(header *h) {
     int index = h->h_size/PAGESIZE;

     if (index > BIG_BLOCK) index = BIG_BLOCK;

     DEBUG_PRINT('l', ("Make free %p %#x (free list %d)\n", 
		       h->h_memory, h->h_size, index));

     h->h_objsize = 0;
     insert(free_list[index], h);
}

/* free_block -- free a block, merging it with its neighbours */
static header *free_block(header *h, mybool mapped) {
     /* Mapped is true if this memory is being recycled: it's already
        in the page table, but we'll need to zero it. */

     header *prev = left_neighbour(h), *next = right_neighbour(h);

     /* Base and size of area where page table needs updating */
     uchar *pg_memory = h->h_memory;
     unsigned pg_size = (mapped ? 0 : h->h_size);

#ifdef TRACE
     if (debug['l']) {
	  printf("Freeing block at %p, size %#x\n", 
		 h->h_memory, h->h_size);

	  if (prev == NULL) 
	       printf("prev=null, "); 
	  else 
	       printf("prev=%p, ", prev->h_memory);

	  if (next == NULL) 
	       printf("next=null\n"); 
	  else 
	       printf("next=%p\n", next->h_memory);
     }
#endif

     if (mapped) memset(h->h_memory, 0, h->h_size);

     if (prev != NULL && prev->h_objsize == 0) {
	  DEBUG_PRINT('l', ("Merging with prev\n"));
	  unlink(prev);
	  prev->h_size += h->h_size;
	  pg_memory = h->h_memory;
	  pg_size = h->h_size;
	  free_header(h);
	  h = prev;
     }

     if (next != NULL && next->h_objsize == 0) {
	  DEBUG_PRINT('l', ("Merging with next\n"));
	  unlink(next);
	  next->h_memory = h->h_memory;
	  next->h_size += h->h_size;
	  pg_memory = h->h_memory;
	  pg_size = h->h_size;
	  free_header(h);
	  h = next;
     }

     if (pg_size > 0) page_setup(pg_memory, pg_size, h);
     make_free(h);

     /* Return the merged block */
     return h;
}

/* find_block -- find a free block of specified size */
static header *find_block(unsigned size, unsigned objsize) {
     header *h = NULL;
     int i = min(size/PAGESIZE, BIG_BLOCK);

     ASSERT(size % PAGESIZE == 0);

     do {
	  for (headers(h2, free_list[i])) {
	       /* This always succeeds for small blocks, and gives
		  first-fit allocation for big blocks. */
	       if (size <= h2->h_size) {
		    h = h2; break;
	       }
	  }
	  i++;
     } while (h == NULL && i <= BIG_BLOCK);

     if (h == NULL) {
	  /* No suitable block was found.  Get a big chunk. */
	  unsigned chunk = max(size, CHUNK_SIZE);
	  GC_TRACE("[ex]");
	  ASSERT(chunk % PAGESIZE == 0);
	  h = alloc_header();
	  h->h_memory = (uchar *) get_memory(chunk);
	  h->h_size = chunk;
	  /* Add to the free list for merging and page table setup */
	  h = free_block(h, FALSE);
     }

     ASSERT(h->h_memory != NULL && h->h_size >= size);
     unlink(h);

     if (size < h->h_size) {
	  /* Split the block, and return the waste to the free
	     list.  It's best to use header h for the waste: that
	     way, we don't have to reset lots of page table
	     entries when we chip a small piece off a big block. */
	  header *h2 = alloc_header();
	  h2->h_memory = h->h_memory;
	  h2->h_size = size;
	  page_setup(h2->h_memory, size, h2);
		   
	  h->h_memory += size;
	  h->h_size -= size;
	  make_free(h);

	  h = h2;
     }

     h->h_objsize = objsize;
     h->h_epoch = gencount;
     return h;
}


/* OBJECT SIZES */

/* Requests are always rounded up to a whole number of words. Those of
   size <= MAX_SMALL_BYTES are further rounded up to one of a small
   number of sizes from the array size_bytes, and bigger requests are
   rounded up to a number of whole pages.  The size_map table gives
   the appropriate index into size_bytes for each small size in words.
   Thus size_bytes[size_map[s]-1] < 4*s <= size_bytes[size_map[s]] for
   each index s up to half the page size in words.  The sizes are
   almost all multiples of 16 bytes to help with cache alignment.
   Small objects of size size_bytes[i] are allocated by splitting up a
   block of size size_block[i]. */

#define N_SIZES (2*LOG_PAGESIZE)

#ifdef MULTIBLOCKS
#define MAX_SMALL_WORDS (4*(PAGE_WORDS/3))
#else
#define MAX_SMALL_WORDS (PAGE_WORDS/2)
#endif
#define MAX_SMALL_BYTES (BYTES_PER_WORD * MAX_SMALL_WORDS)

static unsigned n_sizes;

static unsigned size_bytes[N_SIZES];
#define pool_size(i) size_bytes[i]

#ifdef MULTIBLOCKS
static unsigned size_block[N_SIZES];
#define pool_block(i) size_block[i]
#else
#define pool_block(i) PAGESIZE
#endif

#define pool_count(i) (pool_block(i) / pool_size(i))

static unsigned char size_map[MAX_SMALL_WORDS+1];
#define pool_map(size) size_map[(size)/BYTES_PER_WORD]

#define GRANULE 16	/* Should be about the size of a cache line */

/* new_size -- adjust and register an object size */
static void new_size(int size, int block) {
     ASSERT(n_sizes < N_SIZES);

     /* Round up while same number will fit in a block */
     size = block / (block / size); 

     /* Round down to a multiple of GRANULE */
     if (size >= GRANULE) size = GRANULE * (size / GRANULE);

     size_bytes[n_sizes] = size; 
#ifdef MULTIBLOCKS
     size_block[n_sizes] = block;
#endif
     n_sizes++;
}     

static void init_sizes(void) {
     /* Establish size_bytes and size_map.  Single-word objects
        (containing only a descriptor) are not allowed, because we
        need to assume that a pointer to the object itself, i.e. to
        the word after the descriptor, is still inside the object. The
        sequence is 2, 4, 8, 12, 16, 24, 32 ... words, rounded up to
	the biggest multiple of GRANULE that allows the same number
	of objects in a page. */

     unsigned k;

     n_sizes = 0; 
     new_size(8, PAGESIZE);
     new_size(16, PAGESIZE);
     k = 16;
     while (k < PAGESIZE/8) {
	  new_size(2*k, PAGESIZE);
	  new_size(3*k, PAGESIZE);
	  k *= 2;
     }

     /* Then ... 1/4, 1/3, 1/2, 2/3, 1, 4/3 pages.  The larger sizes
        are enabled only if MULTIBLOCKS is defined; the extra cost in
        compaction overhead may not be worth the reduction in internal
        fragmentation that is achieved. */
     new_size(PAGESIZE/4, PAGESIZE);
     new_size(PAGESIZE/3, PAGESIZE);
     new_size(PAGESIZE/2, PAGESIZE);
#ifdef MULTIBLOCKS
     new_size(2*PAGESIZE/3, 2*PAGESIZE);
     new_size(PAGESIZE, PAGESIZE);
     new_size(4*PAGESIZE/3, 4*PAGESIZE);
#endif

     ASSERT(size_bytes[n_sizes-1] == MAX_SMALL_BYTES);

     k = 0;
     for (int i = 0; i < n_sizes; i++)
	  while (k * BYTES_PER_WORD <= size_bytes[i]) size_map[k++] = i;

     ASSERT(size_map[MAX_SMALL_WORDS] == n_sizes-1);
}


/* UPPER-LEVEL ALLOCATOR */

/* For each small size, there is a doubly-linked pool of pages
   containing objects of that size, and a separate pool for large
   objects.  A second set of pools is used during garbage collection.
   The blocks in a pools are not necessarily sorted by address. */
static header *block_pool[N_SIZES+1], *old_pool[N_SIZES+1];

/* The free storage in each pool is in the upper part of one of the
   last block of the pool. */
static uchar *free_ptr[N_SIZES+1]; /* First free object */
static int free_count[N_SIZES+1]; /* Number of free objects */

/* To allocate an object of a given size, we first round up the size,
   then look at the free storage in the pool for that size.  If there
   is none, then we try to add a free block to the pool.  But if this
   semispace is full, then we must either run the collector or expand
   the semispace (or maybe both).  We expand the semispace immediately
   if the amount of storage allocated since the last collection is
   less than THRESHOLD times the heap size.

   Running the collector may yield free space in the relevant pool,
   and may yield one or more free blocks; so afterwards we try the
   whole allocation process again. */

#define THRESHOLD 0.5

mybool gcflag = TRUE;
static unsigned alloc_since_gc = 0;
static unsigned pool_total = 0;	    /* Total size of all pools */
static unsigned heap_size = 0;	    /* Size of one semispace */

/* scavenge -- run the collector or expand the heap */
void scavenge(value *sp, unsigned size) {
     if (gcflag && heap_size > 0 
	 && alloc_since_gc > THRESHOLD * heap_size)
	  gc_collect(sp);
     else
	  heap_size += round_up(size, PAGESIZE);
}

static void add_block(int index) {
     header *h = find_block(pool_block(index), pool_size(index));
     insert(block_pool[index], h);
     pool_total += pool_block(index);
     free_ptr[index] = h->h_memory;
     free_count[index] = pool_count(index);
}

void *gc_alloc(value *desc, unsigned size, value *sp) {
     unsigned alloc_size;
     word *p = NULL;
     header *h;

     if (debug['z']) gc_collect(sp);

     size = round_up(size+4, BYTES_PER_WORD);

     if (size <= MAX_SMALL_BYTES) {
	  /* Try to allocate from the appropriate pool */
	  unsigned index = pool_map(size);
	  alloc_size = pool_size(index);
	  ASSERT(alloc_size >= size);

	  if (free_count[index] == 0) {
	       while (pool_total + pool_block(index) > heap_size
		      && free_count[index] == 0)
		    scavenge(sp, pool_block(index));

	       if (free_count[index] == 0)
		    add_block(index);
	  }

	  p = (word *) free_ptr[index];
	  free_ptr[index] += alloc_size;
	  free_count[index]--;
     } else {
	  /* Allocate whole pages */
	  alloc_size = round_up(size, PAGESIZE);

	  while (pool_total + alloc_size > heap_size)
	       scavenge(sp, alloc_size);

	  h = find_block(alloc_size, alloc_size);
	  insert(block_pool[n_sizes], h);
	  pool_total += alloc_size;
	  p = (word *) h->h_memory;
     }

     alloc_since_gc += alloc_size;
     DEBUG_PRINT('c', ("[Alloc %d %p]", size, p));
     *p = address(desc);
     return p+1;
}


/* GARBAGE COLLECTOR */

/* Now it's time to tackle the toughest part: the garbage collector
   itself.  We use a stop-and-copy method, refined to deal with the
   allocation of different sizes of objects from different pages.
   Garbage collection works by copying needed objects out of the old
   heap space into a new space.  When an object is copied, its
   descriptor gets overwritten with the BROKEN_HEART token, and the
   second word gives the location of the copy.  Big objects that
   occupy a block to themselves are not copied but just linked into
   the new pool. */

#define BROKEN_HEART 0xbabeface

#define get_word(p, i) (((word *) p)[i])
#define desc(p) ptrcast(word, get_word(p, 0))

/* redirect -- translate pointer into new space */
static void redirect(word *p) {
     uchar *q, *r, *s;
     header *h;
     int index;

     q  = ptrcast(uchar, *p); /* q is the old pointer value */
     if (q == NULL) return;
     h = get_header(q);
     if (h == NULL) return;	/* Not in the managed heap */
     ASSERT(h->h_objsize > 0);

     if (h->h_objsize <= MAX_SMALL_BYTES) {
	  /* A small object */
	  index = pool_map(h->h_objsize);
	  ASSERT(pool_size(index) == h->h_objsize);
	  r = h->h_memory + round_down(q - h->h_memory, h->h_objsize);
	  /* r is the start of the object containing q */

	  if (get_word(r, 0) == BROKEN_HEART) 
	       s = ptrcast(uchar, get_word(r, 1));
	  else {
	       /* Evacuate object at r */
	       if (free_count[index] == 0) add_block(index);
	       s = free_ptr[index];
	       memcpy(s, r, pool_size(index));
	       free_ptr[index] += pool_size(index);
	       free_count[index]--;
	       get_word(r, 0) = BROKEN_HEART;
	       get_word(r, 1) = address(s);
	  }
	  /* s is the new location for the object r */
	  *p = address(s + (q - r));
     } else if (h->h_epoch < gencount) {
	  /* A big block, not already moved to the new semispace */
	  unlink(h);
	  insert(block_pool[n_sizes], h);
	  h->h_epoch = gencount;
     }
}

/* map_next -- skip over a map item */
static int *map_next(int *p) {
     if (*p % 4 == 0)
	  return p+1;		/* A pointer offset */
     
     switch (*p >> 2) {
     case GC_BASE >> 2:
     case GC_MAP >> 2:
	  return p+2;

     case GC_REPEAT >> 2:
     case GC_FLEX >> 2:
	  p += 4;
	  while (*p != GC_END) p = map_next(p);
	  return p+1;

     case GC_BLOCK >> 2:
	  return p+3;
			 
     case GC_END >> 2:
	  return p+1;

     default:
	  panic("*bad map code %d", *p);
	  return NULL;
     }
}

/* redir_map -- interpret a pointer map, redirecting each pointer */
static void redir_map(unsigned map, uchar *base, int bmshift) {
     int count, stride, op, ndim;
     uchar *base2;
     int *p;

     if (map == 0) return;

     if ((map & 0x1) != 0) {
	  /* A bitmap */
	  int i = -bmshift; 

          map >>= 1;
	  while (map != 0) {
	       if ((map & 0x1) != 0)
		    redirect((word *) &get_word(base, i));
	       i++; map >>= 1;
	  }

	  return;
     }

     p = ptrcast(int, map);

     for (;;) {
	  if (*p % 4 == 0) {
	       /* A pointer offset */
	       redirect((word *) (base + *p));
	       p++;
	       continue;
	  }

	  switch (op = *p) {
	  case GC_BASE:
	       base = ptrcast(uchar, p[1]);
	       break;

	  case GC_REPEAT:
	       base2 = base + p[1];
	       count = p[2];
	       stride = p[3];

	       ASSERT(count > 0);

	       for (int i = 0; i < count; i++)
		    redir_map(address(p+4), base2 + i*stride, 0);

	       break;

	  case GC_BLOCK:
	       base2 = base + p[1];
	       count = p[2];

	       for (int i = 0; i < count; i++)
		    redirect((word *) &get_word(base2, i));
	       break;
			 
	  case GC_MAP:
	       redir_map((unsigned) p[1], base, 0);
	       break;

	  case GC_FLEX:
	       base2 = base + p[1];
	       ndim = p[2];
	       stride = p[3];

	       count = 1;
	       for (int i = 0; i < ndim; i++) 
		    count *= get_word(base2, i+1);
	       
	       base2 = ptrcast(uchar, get_word(base2, 0));
	       for (int i = 0; i < count; i++)
		    redir_map(address(p+4), base2 + i*stride, 0);

	       break;

	  case GC_END:
	       return;

	  default:
	       panic("*bad map code %d", op);
	  }

	  p = map_next(p);
     }
}

/* traverse_stack -- chain down the stack, redirecting in each frame */
static void traverse_stack(value *xsp) {
     value *sp = NULL;
     value pc; pc.i = 0;

     for (value *f = xsp; f != NULL; f = valptr(f[BP])) {
	  value *c = valptr(f[CP]);
#ifdef TRACE
	  proc x = find_proc(c);
#endif

	  /* Local variables and parameters */
	  DEBUG_PRINT('m', ("\nFrame for %s", x->p_name));
	  if (c[CP_MAP].i != 0) 
	       redir_map(c[CP_MAP].i, (uchar *) f, FRAME_SHIFT);

	  if (pc.i != 0) {
	       /* Evaluation stack: look up calling PC value in
		  stack map table. */
	       if (interpreted(c)) {
		    value *r = valptr(c[CP_STKMAP]);
		    if (r == NULL) continue;
		    DEBUG_PRINT('m', ("\n<SM pc=%p>", pointer(pc)));
		    while (pointer(r[0]) != NULL) {
			 DEBUG_PRINT('m', (" %p", pointer(r[0])));
			 if (pointer(r[0]) == pointer(pc)) break;
			 r += 2;
		    }
		    if (pointer(r[0]) != NULL) {
			 DEBUG_PRINT('m', ("\nEval stack (%#x)", r[1].i));
			 redir_map(r[1].i, (uchar *) sp, 0);
		    }
	       } else {
		    /* Compiled primitive: f[PC].i is stack map */
		    DEBUG_PRINT('m', ("\nEval stack (%#x)", pc.i));
		    redir_map(pc.i, (uchar *) sp, 0);
	       }
	  }

	  pc = f[PC]; sp = f + HEAD;
     }
}     
     
/* migrate -- redirect within the heap, recursively copying to new space */
static void migrate(void) {
     header *thumb[N_SIZES], *big_thumb = block_pool[n_sizes];
     uchar *finger[N_SIZES];
     mybool changed;

     /* For each pool, we keep a 'thumb' pointing to one of the blocks
	in the pool, and a 'finger' pointing somewhere in that block.
	We're up to date with the pool when the finger coincides with
	the free pointer for the pool: that implies that the thumb is
	on the last block.  Otherwise, we must check whether the
	finger has reached the end of the block, and if so move to a
	new block.  The free pointer is never at the start of a block,
	so we can be sure there is work to do.  The migration process
	is finished when we're up to date with all the pools.

	For initialisation, we set the thumb to point to the list
	header, and also set the finger to NULL.  For an empty pool,
	free_ptr is NULL too, so that makes us up to date.  After any
	change, we must check all pools again in case more objects
	have migrated into the new space. */

     for (int i = 0; i < n_sizes; i++) {
	  thumb[i] = block_pool[i];
	  finger[i] = NULL;
     }

     do {
	  changed = FALSE;

	  for (int i = 0; i < n_sizes; i++) {
	       while (finger[i] != free_ptr[i]) {
		    if (thumb[i] == block_pool[i]
			|| finger[i] + pool_size(i) 
				> thumb[i]->h_memory + pool_block(i)) {
			 thumb[i] = thumb[i]->h_next;
			 finger[i] = thumb[i]->h_memory;
		    }

		    changed = TRUE;
		    uchar *p = finger[i];
		    if (desc(p) != NULL)
			 redir_map(desc(p)[DESC_MAP], p + BYTES_PER_WORD, 0);
		    finger[i] = p + pool_size(i);
	       }
	  }

	  while (big_thumb->h_next != block_pool[n_sizes]) {
	       changed = TRUE;
	       big_thumb = big_thumb->h_next;
	       uchar *p = big_thumb->h_memory;
	       if (desc(p) != NULL)
		    redir_map(desc(p)[DESC_MAP], p + BYTES_PER_WORD, 0);
	  }
     } while (changed);
}

#ifdef HAVE_SIGPROCMASK
#include <signal.h>

static sigset_t oldmask;

/* mask_signals -- block all signals */
static void mask_signals(void) {
     sigset_t mask;
     sigfillset(&mask);
     sigprocmask(SIG_SETMASK, &mask, &oldmask);
}

/* unmask_signals -- restore the old signal mask */
static void unmask_signals(void) {
     sigprocmask(SIG_SETMASK, &oldmask, NULL);
}
#endif

#ifdef WINDOWS
#define mask_signals()
#define unmask_signals()
#endif

void gc_dump(void) {
     unsigned i;
     unsigned total, small_total = 0, big_total = 0, free_total = 0;

     printf("Active blocks\n");
     for (i = 0; i < n_sizes; i++) {
	  if (!empty(block_pool[i])) {
	       total = 0;
	       printf("  %4d:", pool_size(i));
	       for (headers(h, block_pool[i])) {
		    ASSERT(h->h_memory != NULL && h->h_objsize == pool_size(i));
		    printf(" %p", h->h_memory);
		    total += h->h_size;
	       }
	       printf(" total %#x\n", total);
	       small_total += total;
	  }
     }
     if (!empty(block_pool[n_sizes])) {
	  printf("Big blocks:");
	  for (headers(h, block_pool[n_sizes])) {
	       ASSERT(h->h_memory != NULL && h->h_objsize == h->h_size);
	       printf(" %p (%#x)", h->h_memory, h->h_size);
	       big_total += h->h_size;
	  }
     }
     printf("\n");

     printf("Free block list\n");
     for (i = 1; i <= BIG_BLOCK; i++) {
	  if (!empty(free_list[i])) {
	       if (i == BIG_BLOCK)
		    printf("  Big:");
	       else
		    printf("  %4d:", i);

	       for (headers(h, free_list[i])) {
		    ASSERT(h->h_objsize == 0);
		    printf(" %p (%#x)", h->h_memory, h->h_size);
		    free_total += h->h_size;
	       }

	       printf("\n");
	  }
     }
     printf("\n");

     printf("Small:  %10u\n", small_total);
     printf("Big:    %10u\n", big_total);
     printf("Heap:   %10u", pool_total);
     if (small_total + big_total != pool_total) printf(" (oops)");
     printf("\n");
     printf("Free:   %10u\n", free_total);
}

void gc_collect(value *sp) {
     if (!gcflag) return;

     GC_TRACE("[gc");
     mask_signals();
     gencount++;
     pool_total = 0;

     /* Flip semispaces */
     for (int i = 0; i <= n_sizes; i++) {
	  header *h = block_pool[i];
          block_pool[i] = old_pool[i]; old_pool[i] = h;
	  ASSERT(empty(block_pool[i]));
	  free_ptr[i] = NULL; free_count[i] = 0;
     }

     redir_map(address(gcmap), NULL, 0);  /* Redirect global variables */
     traverse_stack(sp);	/* Redirect pointers in the stack */
     migrate();			/* Redirect internal pointers */

     /* Free old semispace */
     for (int i = 0; i <= n_sizes; i++) {
	  while (! empty(old_pool[i])) {
	       header *h = old_pool[i]->h_next;
	       unlink(h);
	       free_block(h, TRUE);
	  }
     }

     unmask_signals();
     alloc_since_gc = 0;
     GC_TRACE("]");
}

/* gc_init -- initialise everything */
void gc_init(void) {
     unsigned i;

     ASSERT(sizeof(page_index) == PAGESIZE);

     empty_index = (page_index *) scratch_alloc(sizeof(page_index));
     for (i = 0; i < TOP_SIZE; i++) page_table[i] = address(empty_index);

     init_sizes();

     /* Set up list headers */
     for (i = 0; i <= BIG_BLOCK; i++) free_list[i] = new_list();

     for (i = 0; i <= n_sizes; i++) {
	  block_pool[i] = new_list();
	  old_pool[i] = new_list();
     }

     heap_size = INIT_SIZE;
}


/* gc_debug -- set debugging flags */
void gc_debug(char *flags) {
     int i;

     for (i = 0; flags[i] != '\0'; i++)
	  debug[(uchar) flags[i]] = TRUE;
}

/* gc_alloc_size -- return allocated size of an object */
int gc_alloc_size(void *p) {
     header *h = get_header(p);
     return h->h_objsize;
}

int gc_heap_size() {
     return heap_size;
}
