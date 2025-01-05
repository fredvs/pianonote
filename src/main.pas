unit main;

{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
interface

uses
  msetypes,
  mseglob,
  mseguiglob,
  mseguiintf,
  mseapplication,
  msestat,
  msemenus,
  msegui,
  msegraphics,
  msegraphutils,
  mseevent,
  mseclasses,
  msewidgets,
  mseforms,
  mseact,
  msedataedits,
  msedropdownlist,
  mseedit,
  mseificomp,
  mseificompglob,
  mseifiglob,
  Math,
  msestatfile,
  msestream,
  SysUtils,
  msesimplewidgets,
  msedispwidgets,
  mserichstring,
  msebitmap;

type
  tmainfo = class(tmainform)
    tstringedit1: tstringedit;
    tbutton1: TButton;
    tstringdisp1: tstringdisp;
    trealedit1: trealedit;
    timagelist5: timagelist;
    tframecomp2: tframecomp;
    tfacecomp3: tfacecomp;
    trealedit2: trealedit;
    tstringdisp2: tstringdisp;
    tstringdisp3: tstringdisp;
    tbutton2: TButton;
    tstringedit2: tstringedit;
    tstringdisp4: tstringdisp;
    procedure onexec(const Sender: TObject);
    procedure oncreaex(const Sender: TObject);
    procedure onfreqex(const Sender: TObject);
  end;

const
  A4_Freq        = 440.0;
  NotesPerOctave = 12;
  NoteNames: array[0..11] of string = ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
  NoteNamesSolfege: array[0..11] of string = ('do', 'do#', 're', 're#', 'mi', 'fa', 'fa#', 'sol', 'sol#', 'la', 'la#', 'si');

var
  mainfo: tmainfo;
  root12of2, noteFrequency: real;
  noteName, notePart, solfNote: string;
  semitoneOffset, octave: integer;
  err: byte = 1;

implementation

uses
  main_mfm;

{ Function to convert solfège syllable (do, re, mi, etc.) to standard note name (C, D, E, etc.) }
function SolfegeToNote(solf: string): string;
var
  solfn, accid: string;
begin
  if (solf[length(solf)] = '#') or (solf[length(solf)] = 'b') then
  begin
    accid := solf[length(solf)];
    solfn := lowercase(copy(solf, 1, length(solf) - 1));
  end
  else
  begin
    accid := '';
    solfn := lowercase(solf);
  end;

  err := 0;

  if solfn = 'do' then
    SolfegeToNote := 'C' + accid
  else if (solfn = 're') or (solfn = 'ré') then
    SolfegeToNote := 'D' + accid
  else if solfn = 'mi' then
    SolfegeToNote := 'E' + accid
  else if solfn = 'fa' then
    SolfegeToNote := 'F' + accid
  else if solfn = 'sol' then
    SolfegeToNote := 'G' + accid
  else if solfn = 'la' then
    SolfegeToNote := 'A' + accid
  else if solfn = 'si' then
    SolfegeToNote := 'B' + accid
  else
  begin
    writeln('Error: Invalid solfège syllable!');
    err           := 1;
  end;
end;

{ Function to check if the note has an accidental (sharp or flat) }
function IsAccidental(note: string): Boolean;
begin
  Result := (note[length(note)] = '#') or (note[length(note)] = 'b');
end;

{ Function to convert note name (with optional sharp or flat) to semitone offset from A4 }
function GetSemitoneOffset(note: string; octave: integer): integer;
var
  noteBaseOffset: integer;
begin
  err   := 0;
  { Base semitone offsets relative to A4 (using C0 as reference) }
  if uppercase(note[1]) = 'C' then
    noteBaseOffset := -9  { C is 9 semitones below A in the same octave }
  else if uppercase(note[1]) = 'D' then
    noteBaseOffset := -7
  else if uppercase(note[1]) = 'E' then
    noteBaseOffset := -5
  else if uppercase(note[1]) = 'F' then
    noteBaseOffset := -4
  else if uppercase(note[1]) = 'G' then
    noteBaseOffset := -2
  else if uppercase(note[1]) = 'A' then
    noteBaseOffset := 0
  else if uppercase(note[1]) = 'B' then
    noteBaseOffset := 2
  else
  begin
    err := 1;
    mainfo.tstringdisp1.Text := 'Error: Invalid note name!';
  end;

  if err = 0 then
  begin
    { Adjust for accidentals (sharp '#' or flat 'b') if applicable }
    if IsAccidental(note) then
      if note[length(note)] = 'b' then
        noteBaseOffset := noteBaseOffset - 1  { Flat lowers by 1 semitone }
      else if note[length(note)] = '#' then
        noteBaseOffset := noteBaseOffset + 1{ Sharp raises by 1 semitone };

    { Calculate total semitone offset relative to A4 }
    GetSemitoneOffset := noteBaseOffset + (octave - 4) * 12;
  end;
end;

  // Function to calculate the frequency of a note based on its distance from A4
function GetFrequencyFromNoteIndex(NoteIndex: integer): double;
begin
  // Calculate the frequency of the note n semitones away from A4
  Result := A4_Freq * Power(2, NoteIndex / NotesPerOctave);
end;

// Function to find the nearest note and the percentage difference in tuning
procedure FindNearestNote(Frequency: double);
var
  NoteIndex, Octave, NearestNoteIndex: integer;
  NearestNoteFreq, DiffPercent: double;
begin
  // Calculate the note index relative to A4
  NoteIndex := Round(NotesPerOctave * Log2(Frequency / A4_Freq));

  // Calculate the frequency of the nearest note based on the note index
  NearestNoteFreq := GetFrequencyFromNoteIndex(NoteIndex);

  // Calculate the percentage difference
  DiffPercent := ((Frequency - NearestNoteFreq) / NearestNoteFreq) * 100; // Keep sign for sharp/flat indication

  // Adjust index to match a note in the piano scale (mod 12)
  NearestNoteIndex   := (NoteIndex + 9) mod NotesPerOctave;
  if NearestNoteIndex < 0 then
    NearestNoteIndex := NearestNoteIndex + NotesPerOctave;

  // Calculate the octave number (starting from A4 = 440Hz at octave 4)
  Octave := 4 + (NoteIndex div NotesPerOctave);

  // Output the nearest note and tuning percentage
  mainfo.tstringdisp4.Text := 'Nearest note: ' + NoteNames[NearestNoteIndex] + IntToStr(Octave) + ' / ' +
    NoteNamesSolfege[NearestNoteIndex] + IntToStr(Octave);

  if Abs(Frequency - NearestNoteFreq) < 0.01 then
    mainfo.tstringdisp4.Text := mainfo.tstringdisp4.Text + #10 + 'Exact frequency match!'
  else
    mainfo.tstringdisp4.Text := mainfo.tstringdisp4.Text + #10 + 'Tuning needed: ' + FloatToStrF(DiffPercent * -1, ffFixed, 8, 2) + '%';

  mainfo.tstringedit2.Text := NoteNames[NearestNoteIndex] + IntToStr(Octave) + ' / ' +
    NoteNamesSolfege[NearestNoteIndex] + IntToStr(Octave);

end;

procedure tmainfo.onexec(const Sender: TObject);
begin
  err      := 0;
  noteName := trim(tstringedit1.Value);

  if ((length(noteName) = 2) and (noteName[2] in ['0'..'9'])) or
    ((length(noteName) = 3) and (noteName[3] in ['0'..'9']) and (noteName[2] = '#')) or
    ((length(noteName) = 3) and (noteName[3] in ['0'..'9']) and (lowercase(noteName[2]) = 'b')) then
    notePart := copy(noteName, 1, length(noteName) - 1){ Extract note part (e.g., A or A#) }
  else
  begin
    solfNote := copy(noteName, 1, length(noteName) - 1);  { Extract solfège syllable (first two characters) }
    notePart := SolfegeToNote(solfNote);  { Convert to standard note notation }
    noteName := notePart + copy(noteName, length(noteName), 1);  { Replace solfège with note name }
  end;

  { Extract octave from the last character }
  if err = 0 then
    octave := StrToInt(noteName[length(noteName)]);  { Last character is the octave number }

  { Calculate the semitone offset from A4, based on the note name and octave }
  if err = 0 then
    semitoneOffset := GetSemitoneOffset(notePart, octave);

  { Calculate the frequency of the note using the semitone offset }
  if err = 0 then
    noteFrequency := A4_Freq * exp(ln(root12of2) * semitoneOffset);

  { Output the result }
  if err = 0 then
  begin
    trealedit1.Value  := RoundTo(noteFrequency, -2);
    tstringdisp1.Text := 'The frequency of ' + noteName + ' is: ' + FloatToStrF(noteFrequency, ffFixed, 8, 2) + ' Hz';
  end;
end;

procedure tmainfo.oncreaex(const Sender: TObject);
begin
  { Calculate the 12th root of 2 }
  root12of2 := exp(ln(2) / 12);
end;

procedure tmainfo.onfreqex(const Sender: TObject);
begin
  err := 0;
  FindNearestNote(trealedit2.Value);
end;

end.

