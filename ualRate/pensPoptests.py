#!/usr/bin/env python3

from pensPop import pensMember


def testdoesMemberRetire():
    counter = 0
    for i in range(100000):
        andy = pensMember(62, "M", 15, 1000, 2005)
        if andy.doesMemberRetire():
            counter += 1
    print(counter)


def testdoesMemberSeparate():
    counter = 0
    for i in range(100000):
        andy = pensMember(62, "M", 15, 1000, 2005)
        if andy.doesMemberSeparate():
            counter += 1
    print(counter)


if __name__ == "__main__":
    testdoesMemberRetire()
    testdoesMemberSeparate()

