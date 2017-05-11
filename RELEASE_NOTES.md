### 3.0.0-beta2
* Change byref matching
* Change the result format
* Add the link generator to .Net Api Browser

### 3.0.0-beta1
* Support struct tuple
* Remove `=>` syntax
* Add C# mode

### 2.1.2 - 2017/1/26
* Fix the computation expression builder url

### 2.1.1 - 2016/12/29
* Fix the distance of swap-order

### 2.1.0 - 2016/12/6
* Support computation expression
* Add swapping parameters and tuple elements
* Add complementing parameters and tuple elements
* Fix type abbreviation result
* Fix automatic generalization parameter
* Improve assembly loading

### 2.0.0 - 2016/10/20
Engine
* Improve the display of the parameter name
* Support optional parameters
* Add `--ignore-case` option
* Add type and type abbreviation searching
* Add module searching
* Add union case searching
* Add convertion between function and delegate
* Change the constructor name query to "T.new : _", "T..ctor : _" and "T : _" from "T : _"
* Add partial matching to name searching

Libarary
* Add FSharpApiSearch.LinkGenerator module
* Change SearchOption's accessors to function

Console
* Add `#clear` command to clear the console buffer
* Add `#targets` command to list target assemblies
* Improve the startup time

### 2.0.0-beta2 - 2016/10/13
Engine
* Add partial matching to name searching
* Add module searching
* Add the namespace to the result of type abbreviation
* Change to allow the type name and ".ctor" to the constructor
* Improve type abbreviation matching with the ignore-case option
* Fix to ignore private type definition

Console
* Add `#clear` command to clear the console buffer
* Improve the startup time

### 2.0.0-beta1 - 2016/9/29
Engine
* Improve the display of the parameter name
* Add `--ignore-case` option
* Add type and type abbreviation searching
* Add conversion between function and delegate
* Support union case
* Support optional parameters
* Change the constructor name query to "new : _"

Library
* Add LinkGenerator module
* Change the SearchOptions's accessor to function

### 1.0.1 - 2016/9/26
* Allow digit, _ and ' in the name search query (#92)

### 1.0.0 - 2016/7/29
* Add the assembly name to the result
* Add the namespace searching (#9)
* Change the search option names (#61)

### 0.4.0-beta - 2016/6/23
* Change the first argument to search as a receiver (#80)
* Add searching by operator name
* Add `--xmldoc` option to show xml document
* Fix loading bugs (#41, #83)
* Fix searching bugs (#78, #68)

### 0.3.1-beta - 2016/6/14
* Fix error at similarity searching (#81)

### 0.3.0-beta - 2016/5/22
* Support active pattern
* Support function type abbreviation
* Support type extension and extension member
* Fix array printing (#57)

### 0.2.1-beta - 2016/5/15
* Fix loading type constraints of type abbreviation (#66)
* Fix FSharpApiSearch.Database.exe option's bugs (#65, #67)

### 0.2.0-beta - 2016/4/27
* Support type constraints at similarity searching
* Exclude the modules (LanguagePrimitives and OperatorIntrinsics) for the F# compiler
* Add ignore-argstyle option
* Improve the start-up time by database

### 0.1.0-beta
* Initial release
