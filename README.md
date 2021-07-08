# matel
![Matel's Glorious Logo](matel-sty01.svg)

Matel is an incomplete Haskell-based Matrix client.

## Goals
The goals of the development of Matel are as follows:
* Matel fully supports Matrix's text-based communication.
* Matel is damn fast.
* Matel's source code is readable.
* Matel runs perfectly on the BSDs, the Linux distros, and Microsoft Windows.
* The number of Matel's dependencies is minimised.

## Pronunciation
The official pronunciations of "Matel" include \/məˈtɛl\/, which is VARIK's preferred pronunciation, and \/meɪˌtəl\/.

## Reporting Bugs
For all new bugs, a bug can be mentioned via the GitHub "Issues" feature or by sending a DETAILED e-mail to Varik Valefor \<varikvalefor@aol.com\> such that the e-mail's subject line contains the phrase "MATEL BUG".

## Contributing

Contributions are welcomed but must be released in accordance with Matel's licence.

For all good changes, the diff of a good change can be pinged to Varik Valefor \<varikvalefor@aol.com\> such that the e-mail's subject line includes the phrase "MATEL CONTRIBUTION", or a pull request for the diff of this good change can be submitted to this GitHub repository.

## Getting Started
### Configuration
#### Required Stuff
The basic information of Matel, e.g., the username and password of the user, are contained within the file `$PATH/.config/matel`.

The username _k_ of the user should be placed onto a line such that the line matches the format `username: `_k_.

The password _l_ of the user should be placed onto a line such that the line matches the format `password: `_l_.

The FQDN _m_ of the user's homeserver should be placed onto a line such that the line matches the format `password: `_m_.

The authorisation token _n_ of Matel should be placed onto a line such that the line matches the format `authtoken: `_n_.
#### Colour
The colours of Matel's TUI can be changed through the editing of the `Colour` module, which is located at `src/Colour.hs`.  Like the rest of Matel, `Colour` has decent documentation and can be edited reasonably easily.
