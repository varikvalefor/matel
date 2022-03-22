# matel
![Matel's Glorious Logo](matel-sty01.svg)

Matel is an incomplete Haskell-based Matrix client.

## Goals
The goals of the development of Matel are as follows:

* Matel fully supports Matrix's text-based communication.

* Matel is damn fast.

* Matel's source code is readable.

* Matel is extensible.

* Matel runs perfectly on the BSDs, the Linux distros, and Microsoft Windows.

* The number of Matel's dependencies is minimised.  (So far, so bad...)

## Pronunciation
The official pronunciations of "Matel" include \/məˈtɛl\/, which is VARIK's preferred pronunciation, and \/meɪˌtəl\/.

## Reporting Bugs

For all new bugs, a bug can be mentioned via the GitHub "Issues" feature or by sending a DETAILED e-mail to Varik "EVERY FILE MUST CONTAIN A JOKE" Valefor \<varikvalefor@aol.com\> such that the e-mail's subject line contains the phrase "MATEL BUG".

## Contributing

Contributions are welcomed but must be released in accordance with Matel's licence.

For all good changes, the diff of a good change can be pinged to Varik Valefor \<varikvalefor@aol.com\> such that the e-mail's subject line includes the phrase "MATEL CONTRIBUTION", or a pull request for this good change can be submitted to this GitHub repository.

For all Matel issues, for all men, if a man wishes to fix a Matel issue, then this man should inform Varik Valefor via e-mail or Matrix at varikvalefor@aol.com or @varikvalefor:matrix.org of this man's intent to fix this Matel issue.  If VARIK is informed of this intent via e-mail, then the subject line of this e-mail should include the phrase "MATEL ISSUE ASSIGNMENT REQUEST".

## Getting Started

### Warning

Because Matel is currently in the pre-alpha stage, some updates may break backwards compatibility.

The use of Matel in production environments is not recommended.

### Configuration

#### Required Stuff

The basic information of Matel, e.g., the username and password of the user, are contained within the file `[HOME DIRECTORY]/.config/matel`.

For all lines of this file _g_, _g_ follows the format "[LABEL]: [CONTENT]".  The elements of {\<LABEL NAME, DESCRIPTION OF CONTENT AT LABEL\> : MATEL ACCEPTS LABEL} are as follows:

* \<`username`, the username of the Matrix account which should be used, not including ":foo.bar"\>

* \<`password`, the password of the Matrix account which should be used\>

* \<`homeserver`, the FQDN of the homeserver of the Matrix account which should be used\>

* \<`authtoken`, the authorisation token which Matel uses to act on behalf of the user\>

* \<`protocol`, the protocol which Matel/MATELCLI uses to contact the homeserver of the user\>

#### Colour
The colours of Matel's TUI can be changed through the editing of the `Colour` module, which is located at `app/Colour.hs`.  Like most other modules of Matel, `Colour` has decent documentation and can be modified reasonably easily.

### Using Matel
Matel is incomplete and currently useless.

### Using MATELCLI
Read `matelcli`'s beautifully detailed manual page in `matelcli.1`.

## Officially-Supported Systems
OpenBSD is the primary operating system on which Matel is written and tested, although FreeBSD is used for some testing.

VARIK is willing to implement support for other operating systems but cannot guarantee that arbitrary versions of Matel work on operating systems which are not officially supported.

## Help Wanted
VARIK "NOT A COMPUTER PROGRAMMER!!!" VALEFOR could use some assistance with the implementation of cryptography.

Pull requests are welcomed.

## The Matel Room
There exists a Matrix room such that this Matrix room is dedicated to Matel.  See The Matel Room <`#johnnykissassSuckupfest:matrix.org`>.
