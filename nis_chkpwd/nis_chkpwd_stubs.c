/* NIS password checker using ypmatch
 *
 * Copyright (C) 2008 Stéphane Glondu
 *   (Laboratoire PPS - CNRS - Université Paris Diderot)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  */

#define _XOPEN_SOURCE
#include <unistd.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>

CAMLprim value crypt_stub(value key, value salt)
{
  CAMLparam2(key,salt);
  char *r = NULL;

  if ((r = crypt(String_val(key), String_val(salt))) == NULL) {
    caml_raise_not_found();
  } else {
    CAMLlocal1(result);
    /* WARNING: r points to static data! */
    result = caml_copy_string(r);
    CAMLreturn(result);
  }
}
