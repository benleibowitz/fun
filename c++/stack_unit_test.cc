#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE StackUnitTest
#include <boost/test/unit_test.hpp>
#include "Stack.h"
#include <iostream>
#include <array>
using std::array;
using std::cout;

BOOST_AUTO_TEST_SUITE(StackTestSuite)

BOOST_AUTO_TEST_CASE(TestIsEmpty) {
  array<int, 6> nums = {0, 2, 4, 6, 8, 10};

  //Test a few manually on s1
	Stack s1;
	BOOST_REQUIRE(s1.IsEmpty());
  s1.Pop();
  BOOST_REQUIRE(s1.IsEmpty());
  s1.Push(5);
  BOOST_REQUIRE(!s1.IsEmpty());
  s1.Pop();
  BOOST_REQUIRE(s1.IsEmpty());

  //Test IsEmpty() in loop
  int nums_idx = nums.size() - 1;
  while(!s1.IsEmpty()) {
    int n = s1.Pop();
    BOOST_REQUIRE(n == nums[nums_idx--]);
  }

  //Test Push and Pop with IsEmpty()
	for(int i = 0; i < nums.size(); i++) {
		Stack s2;
    BOOST_REQUIRE(s2.IsEmpty());

		for(int j = i; j >= 0; j--) {
			s2.Push(nums[j]);
			BOOST_REQUIRE(!s2.IsEmpty());
		}

    for(int j = 0; j <= i; j++) {
      int n = s2.Pop();
      BOOST_REQUIRE(n == nums[j]);
    }

    BOOST_REQUIRE(s2.IsEmpty());
	}

  for(int i = 0; i < nums.size(); i++) {
    s1.Push(nums[i]);
  }

}

BOOST_AUTO_TEST_CASE(TestPushPop) {
  const unsigned int SIZE = 10000000;
  
	Stack s;
  BOOST_REQUIRE(s.IsEmpty());

  for(int i = 0; i < SIZE; i++) {
    s.Push(i);
    BOOST_REQUIRE(!s.IsEmpty());
  }

  for(int i = SIZE - 1; i >= 0; i--) {
    int n = s.Pop();
    BOOST_REQUIRE(n == i);
  }
}

BOOST_AUTO_TEST_SUITE_END()
