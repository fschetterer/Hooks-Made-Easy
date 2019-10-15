::::::::::::: Place in All Config all Platform PostBuild :::::::::::::
:: Set PLATFORM=$(Platform)
:: Set OUTPUTPATH=$(OUTPUTPATH)
:: Set OUTPUTNAME=$(OUTPUTNAME)
:: Set OUTPUTEXT=$(OUTPUTEXT)
:: Set OUTPUTDIR=$(OUTPUTDIR) + any redirect like: $(OUTPUTDIR)\..\..
:: "Append Bitness.cmd"
:::::::::::::::::::::::::::: END ::::::::::::::::::::::::::::

:: Strip Bitness from Platform (Win32/Win64)
SET BITNESS=%PLATFORM:~3,2%
:: Copy so the original stays intact for Debugging
COPY /Y "%OUTPUTPATH%" "%OUTPUTDIR%\%OUTPUTNAME%%BITNESS%%OUTPUTEXT%"

