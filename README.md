# Delphi Hooks-Made-Easy
A re-release of updated code originally written some 20 years ago.

## Basics
Keep in mind that in some cases both a 32 and 64 bit version of a DLL are required.
### DLL code 
Projects will compile in Delphi XE2 and up. XE2 was chosen since it will compile for both 32 and 64 bit with a small binary footprint.
### Test Code and Manifests
A manifest file is supplied for Delphi Berlin which includes 'uiAccess'. Newer IDEs have this option built in.
Debugging is best done with a manifest that does not include 'uiAccess'.
'LinkManifests.cmd' will generate symbolic links to the supplied manifest.
### Code Signing
As of Windows 10.1903.18362.295 a Sandbox VM allowed debug testing without code signing. 
### Deployment
Best to change the DLL name, change the Mapfile Filename and follow these steps:
* uiAccess="true"
* Code MUST be digitally signed
* Your application MUST reside in a trusted location (e.g.; Program Files)  

Without this hooking a 'uiAccess' app will fail and possibly lock your App.

## HooksMadeEasy.Journal
Recording and playback of a Journal hook. The code can reside inside an EXE or DLL and works for both 32/64 bit without an additional version. However the way I read this is that its best to use a DLL since non native bitness apps will end up loaded your code via a thunking mechanism.

## HooksMadeEasy.GetMsg
Hooks all messages posted via PostMessage, best is to refine what you need to watch and let the rest flow through.
The current demo simply posts a message back to the original App when a System Menu is clicked.

## Still to come
* Remote Inject API
Injects the DLL into a process using its PID, where a DDetours call
essentially redirects an API.
This could be a Hook process specific to lessen load on the system
when only one process needs to be hooked.
* more..

## See Also
* [How do I disable driver signature enforcement Win 10](https://answers.microsoft.com/en-us/insider/forum/insider_wintp-insider_devices/how-do-i-disable-driver-signature-enforcement-win/a53ec7ca-bdd3-4f39-a3af-3bd92336d248?auth=1)  
* [Windows, Drivers and Digital Signatures](http://blog.morphisec.com/windows-drivers-and-digital-signatures)  
* [Driver Signing changes in Windows 10, version 1607](https://blogs.msdn.microsoft.com/windows_hardware_certification/2016/07/26/driver-signing-changes-in-windows-10-version-1607/)
* [API Hooking using AppInit_DLLs](https://www.apriorit.com/dev-blog/160-apihooks)
* [Disable driver signature enforcement permanently in Windows 10](https://winaero.com/blog/disable-driver-signature-enforcement-permanently-in-windows-10/)
* [Easy shell hooking example](https://www.autoitscript.com/forum/topic/56536-easy-shell-hooking-example/)
* [Using accessibility to monitor windows as they come and go](https://blogs.msdn.microsoft.com/oldnewthing/20130325-00/?p=4863/)

