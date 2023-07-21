This is the "shplait" package, which implements the Shplait language as
used with

```
 #lang shplait
```

as the start of a program module.

The package is meant to be installed with the Racket package manager:

 * In recent versions of DrRacket, choose "Install Package.."
   from the "File" menu, and enter

   ```
   shplait
   ```

   in the dialog.

 * From the comamnd line:

   ```
   raco pkg install shplait
   ```

The Shplait language is based on Plait, but with a shrubbery-based
syntax that resembles Rhombus. Plait is, in turn, based on
"plai-typed", which is based on the "plai" language, which originates
from the book "Programming Languages: Application and Interpretation"
by Shriram Krishnamurthi.

If you're interested in how Shplait is implemented, see
[IMPLEMENTATION.md](IMPLEMENTATION.md).
