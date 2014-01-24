/*
 * Topological sort
 */
#include "config.h"
#include "utils.h"
#include <pool.h>
#include <hash.h>
#include <list.h>

static poolPo groupPool;
static poolPo definePool;

Iterator topological(Iterator elements)
{
  listPo definitions = findDefinitions(elements);
  hashPo index = buildIndex(definitions);
  listPo groups = sort(definitions,index);
  return listIterator(groups);
}
