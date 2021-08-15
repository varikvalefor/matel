# matel
![Matel's Glorious Logo](matel-sty01.svg)

Matel is an incomplete Haskell-based Matrix client.

## Goals
The goals of the development of Matel are as follows:
* Matel fully supports Matrix's text-based communication.
* Matel is damn fast.
* Matel's source code is readable.
* Matel runs perfectly on the BSDs, the Linux distros, and Microsoft Windows.
* The number of Matel's dependencies is minimised.  (So far, so bad...)

## Pronunciation
The official pronunciations of "Matel" include \/məˈtɛl\/, which is VARIK's preferred pronunciation, and \/meɪˌtəl\/.

## Reporting Bugs
For all new bugs, a bug can be mentioned via the GitHub "Issues" feature or by sending a DETAILED e-mail to Varik Valefor \<varikvalefor@aol.com\> such that the e-mail's subject line contains the phrase "MATEL BUG".

## Contributing

Contributions are welcomed but must be released in accordance with Matel's licence.

For all good changes, the diff of a good change can be pinged to Varik Valefor \<varikvalefor@aol.com\> such that the e-mail's subject line includes the phrase "MATEL CONTRIBUTION", or a pull request for the diff of this good change can be submitted to this GitHub repository.

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
#### Colour
The colours of Matel's TUI can be changed through the editing of the `Colour` module, which is located at `app/Colour.hs`.  Like most other modules of Matel, `Colour` has decent documentation and can be edited reasonably easily.

## Commit Labels
### Untested Commits and "(U)"
For all untested commits, the commit message of an untested commit should begin with "(U)".
