algo
====

A general content management core that makes it easy to model any 'thing' (algo in Spanish)


# Description of the versioning system.

The version list is kept such that the 'effective version' always
comes first in the list and the arrangements of maintaining the
effective version first is done when adding a new version - never
when retrieving the version.

By 'effective version' is meant, by example:

a. Suppose you have two preliminary versions and no released
   version yet. In this case PA2, the latest preliminary version,
   is the effective version

      PA2 <- PA1        

b. Suppose you have a released version, A, with two preceeding
   preliminary versions PA1 to PA2. In this case A is the effective
   version.

      A <- PA2 <- PA1

c. Suppose you have a released version A, with two preceeding
   preliminary versions PA1 to PA2 and with two subsequent
   preliminiary version PB1 to PB2. In this case A is still
   the effective version.

      A <- PB2 <- PB1 <- PA2 <- PA1

d. Suppose you have a released version B, with two preceeding
   preliminary versions PB1 to PB2 and with a preceeding
   released version A. In this case B is the effective version.

      B <- PB2 <- PB1 <- A <- PA2 <- PA1


Thus;
1. The version most probably retrieved is always stored first.

2. All versions must have unique and increasing versionId's. They
   do not have to be in a strict incremental sequece.

3. Generally the latest version (highest versionId) is stored first
   in the list and the list is sorted on decreasing versionId.

4. If there are no elements with versionState == released in the
   list, the whole list shall be adequately sorted in decreasing
   order on versionId.

5. The first element does not have to be the latest version if it
   has versionState == released, in which case no other released
   versions with higher versionId must appear anywhere in the list.
