# $Id: Makefile.PL,v 1.2 2004/03/05 10:18:53 roel Exp $

use inc::Module::Install;

name            ('Local-MifTree');
abstract        ('A MIF Parser');
author          ('Roel van der Steen <roel-perl@st2x.net>');
version_from    ('lib/Local/MifTree.pm');
license         ('perl');
requires        ('perl' => 5.006);
requires        ('File::Temp');
requires        ('IO::Tokenized');
requires        ('IO::Tokenized::File');
requires        ('IO::Tokenized::Scalar');
requires        ('Tree::DAG_Node');

&WriteAll;
