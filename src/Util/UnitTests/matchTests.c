//
// Created by Francis McCabe on 1/1/22.
//

#include "matchTests.h"
#include <assert.h>

static void setupTests() {
}

static void tearDownTests() {
}

static retCode simpleMatch() {
  assert(strMatch("testing", "testing"));
  assert(!strMatch("testing", "tested"));
  return Ok;
}

static retCode star1Match() {
  assert(strMatch("testing", "test*"));
  assert(strMatch("testing", "test*g"));
  return Ok;
}

static retCode star2Match() {
  assert(strMatch("testing", "t*t*"));
  assert(strMatch("testing", "t*t*g"));
  return Ok;
}

static retCode fontGroupMatch() {
  assert(strMatch("-adobe-courier-bold-o-normal--12-120-75-75-m-70-iso8859-1", "-*-*-*-*-*-*-12-*-*-*-m-*-*-*"));
  assert(!strMatch("-adobe-courier-bold-o-normal--12-120-75-75-X-70-iso8859-1", "-*-*-*-*-*-*-12-*-*-*-m-*-*-*"));
  return Ok;
}

retCode matchTests() {
  setupTests();
  tryRet(run_test(simpleMatch));
  tryRet(run_test(star1Match));
  tryRet(run_test(star2Match));
  tryRet(run_test(fontGroupMatch));
  tearDownTests();
  return Ok;
}
