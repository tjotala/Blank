BLANK
=====

Application Program Interface (API)

Copyright ©1994 Tapani J. Otala

Introduction
------------

This document describes the application program interface (API) provided by BLANK, the multi-purpose screen saver.  BLANK can be configured to turn off more than one screen, which makes it ideal for dual display systems.

Obtaining Access
----------------

The API is accessible via a FAR call to a entry point.  The following sections describe how to obtain the address of the entry point in various environments.

MS-DOS
------

There are two ways to obtain the entry point address, presented here in the order of preference:
* Multiplex interrupt 0x2D using Alternate Multiplex Interrupt Specification (AMIS).
* Multiplex interrupt 0x2F.

MS-Windows
----------

The entry point can be located using the same methods as described in section MS-DOS, taking into consideration the protected mode vs. real mode issues.
