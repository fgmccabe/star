/*
  This computes integer log2(x) in no more than log2(32) steps

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <fgm@fla.fujitsu.com>
*/

long log2(register long x)
{
  if(x<0x10000)
    if(x<0x100)
      if(x<0x10)
	if(x<0x4)
	  if(x<0x2)
	    return 0;
	  else
	    return 1;
	else
	  if(x<0x8)
	    return 2;
	  else
	    return 3;
      else /* >=0x10 < 0x100 */
	if(x<0x40)
	  if(x<0x20)
	    return 4;
	  else
	    return 5;
	else
	  if(x<0x80)
	    return 6;
	  else
	    return 7;
    else /* >=0x100 < 0x10000 */
      if(x<0x1000)
	if(x<0x400)
	  if(x<0x200)
	    return 8;
	  else
	    return 9;
	else/* x>=0x400 < 0x1000 */
	  if(x<0x800)
	    return 10;
	  else
	    return 11;
      else /* x>=0x1000 < 0x10000 */
	if(x<0x4000)
	  if(x<0x2000)
	    return 12;
	  else
	    return 13;
	else
	  if(x<0x8000)
	    return 14;
	  else
	    return 15;
  else
    if(x<0x1000000)
      if(x<0x100000)
	if(x<0x40000)
	  if(x<0x20000)
	    return 16;
	  else
	    return 17;
	else
	  if(x<0x80000)
	    return 18;
	  else
	    return 19;
      else /* >=0x100000 < 0x1000000 */
	if(x<0x400000)
	  if(x<0x200000)
	    return 20;
	  else
	    return 21;
	else
	  if(x<0x800000)
	    return 22;
	  else
	    return 23;
    else /* x>=0x1000000 <0x80000000 */
      if(x<0x10000000)
	if(x<0x4000000)
	  if(x<0x2000000)
	    return 24;
	  else
	    return 25;
	else
	  if(x<0x8000000)
	    return 26;
	  else
	    return 27;
      else
	if(x<0x40000000)
	  if(x<0x20000000)
	    return 28;
	  else
	    return 29;
	else
	  if(x<0x80000000)
	    return 30;
	  else
	    return 31;
}


