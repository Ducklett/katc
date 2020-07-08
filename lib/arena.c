/**

 * adapted from https://github.com/thejefflarson/arena

 * The MIT License (MIT)

 * Copyright (c) 2015 Jeff Larson

 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include <stdint.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

#define PAGE_SIZE 1024

typedef struct arena {
  uint8_t *region;
  size_t size;
  size_t current;
  struct arena *next;
} arena_t;

arena_t *
arena_create();

void* arena_malloc(arena_t *arena, size_t size);

void arena_destroy(arena_t *arena);

////////////////////////

static arena_t* _arena_create(size_t size) {
  arena_t *arena = (arena_t *) calloc(1, sizeof(arena_t));
  if(!arena) return NULL;
  arena->region = (uint8_t *) calloc(size, sizeof(uint8_t));
  arena->size   = size;
  if(!arena->region) { free(arena); return NULL; }
  return arena;
}

arena_t* arena_create() {
  return _arena_create(PAGE_SIZE);
}

void* arena_malloc(arena_t *arena, size_t size) {
  arena_t *last = arena;

  do {
    if((arena->size - arena->current) > size){
      arena->current += size;
      return arena->region + (arena->current - size);
    }
    last = arena;
  } while((arena = arena->next) != NULL);

  size_t asize   = size > PAGE_SIZE ? size : PAGE_SIZE;
  arena_t *next  = _arena_create(asize);
  last->next     = next;
  next->current += size;
  return next->region;
}

void arena_destroy(arena_t *arena) {
  arena_t *next, *last = arena;
  do {
    next = last->next;
    free(last->region);
    free(last);
    last = next;
  } while(next != NULL);
}