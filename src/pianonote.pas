program pianonote;

{$ifdef FPC} {$mode objfpc}{$h+} {$endif}
uses
 {$ifdef FPC} {$ifdef unix}cthreads, {$endif} {$endif}
  msegui,
  msegraphics,
  msegraphutils,
  main;

begin
  application.createform(tmainfo, mainfo);
  application.run;
end.

