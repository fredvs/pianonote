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
  msebitmap,
  msegraphedits,
  msescrollbar;

type
  tmainfo = class(tmainform)
    ed_notename1: tstringedit;
    bu_inputnote: TButton;
    di_note: tstringdisp;
    ed_frequency1: trealedit;
    timagelist: timagelist;
    tframecomp: tframecomp;
    tfacecomp: tfacecomp;
    ed_frequency2: trealedit;
    pa_name: tstringdisp;
    pa_frequency: tstringdisp;
    bu_inputfreq: TButton;
    ed_notename2: tstringedit;
    di_frequency: tstringdisp;
    bo_usetable: tbooleanedit;
    procedure onexec(const Sender: TObject);
    procedure oncreaex(const Sender: TObject);
    procedure onfreqex(const Sender: TObject);
  end;

const
  NotesPerOctave = 12; { Number of semitones per octave }
  NoteNames: array[0..11] of string = ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
  NoteNamesSolfege: array[0..11] of string = ('do', 'do#', 're', 're#', 'mi', 'fa', 'fa#', 'sol', 'sol#', 'la', 'la#', 'si');
  A4Index        = 9;  { A4 is the 10th note in the scale (0-indexed), hence position 9 in the octave }
  A4Octave       = 4;  { A4 is in the 4th octave (C0 starts at octave 0) }
  A4Freq         = 440.0;

var
  mainfo: tmainfo;
  root12of2, noteFrequency: real;
  noteName, notePart, solfNote: string;
  semitoneOffset, octave: integer;
  err: byte = 1;
  frequencies: array[1..NotesPerOctave, 1..NotesPerOctave] of real;  { 12x12 array for frequencies }
  noteBaseOffset: integer;

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
begin
  err := 0;

  if mainfo.bo_usetable.Value = False then
  begin
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
      mainfo.di_note.Text := 'Error: Invalid note name!';
    end;
  end else  // use pre-calculated table
  begin
   if uppercase(note[1]) = 'C' then
    noteBaseOffset := 0
  else if uppercase(note[1]) = 'D' then
    noteBaseOffset := 2
  else if uppercase(note[1]) = 'E' then
    noteBaseOffset := 4
  else if uppercase(note[1]) = 'F' then
    noteBaseOffset := 5
  else if uppercase(note[1]) = 'G' then
    noteBaseOffset := 7
  else if uppercase(note[1]) = 'A' then
    noteBaseOffset := 9
  else if uppercase(note[1]) = 'B' then
    noteBaseOffset := 10
  else
  begin
    err := 1;
    mainfo.di_note.Text := 'Error: Invalid note name!';
  end;
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
  Result := A4Freq * Power(2, NoteIndex / NotesPerOctave);
end;

// Function to find the nearest note and the percentage difference in tuning
procedure FindNearestNote(Frequency: double);
var
  NoteIndex, Octave, NearestNoteIndex: integer;
  NearestNoteFreq, DiffPercent: double;
begin
  // Calculate the note index relative to A4
  NoteIndex := Round(NotesPerOctave * Log2(Frequency / A4Freq));

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
  mainfo.di_frequency.Text := 'Nearest note: ' + NoteNames[NearestNoteIndex] + IntToStr(Octave) + ' / ' +
    NoteNamesSolfege[NearestNoteIndex] + IntToStr(Octave);

  if Abs(Frequency - NearestNoteFreq) < 0.01 then
    mainfo.di_frequency.Text := mainfo.di_frequency.Text + #10 + 'Exact frequency match!'
  else
    mainfo.di_frequency.Text := mainfo.di_frequency.Text + #10 + 'Tuning needed: ' + FloatToStrF(DiffPercent * -1, ffFixed, 8, 2) + '%';

  mainfo.ed_notename2.Text := NoteNames[NearestNoteIndex] + IntToStr(Octave) + ' / ' +
    NoteNamesSolfege[NearestNoteIndex] + IntToStr(Octave);

end;

procedure tmainfo.onexec(const Sender: TObject);
begin
  err      := 0;
  noteName := trim(ed_notename1.Value);

  if ((length(noteName) = 2) and (noteName[2] in ['0'..'9'])) or
    ((length(noteName) = 3) and (noteName[3] in ['0'..'9']) and (noteName[2] = '#')) or
    ((length(noteName) = 3) and (noteName[3] in ['0'..'9']) and (lowercase(noteName[2]) = 'b')) then
    notePart := copy(noteName, 1, length(noteName) - 1){ Extract note part (e.g., A or A#) }
  else
  begin
    solfNote := copy(noteName, 1, length(noteName) - 1);  { Extract solfège syllable }
    notePart := SolfegeToNote(solfNote);  { Convert to standard note notation }
    noteName := notePart + copy(noteName, length(noteName), 1);  { Replace solfège with note name }
  end;

  { Extract octave from the last character }
  if err = 0 then
    octave := StrToInt(noteName[length(noteName)]);  { Last character is the octave number }

  { Calculate the semitone offset from A4, based on the note name and octave }
  if err = 0 then
    semitoneOffset := GetSemitoneOffset(notePart, octave);

  if bo_usetable.Value = False then
  begin
    { Calculate the frequency of the note using the semitone offset }
    if err = 0 then
      noteFrequency := A4Freq * exp(ln(root12of2) * semitoneOffset);
    { Output the result }
    if err = 0 then
    begin
      ed_frequency1.Value  := RoundTo(noteFrequency, -2);
      di_note.Text := 'The frequency of ' + noteName + ' is: ' + FloatToStrF(noteFrequency, ffFixed, 8, 2) + ' Hz';
    end;
  end
  else
  begin
   if err = 0 then
   begin
    ed_frequency1.Value  := RoundTo(frequencies[octave + 1, noteBaseOffset + 1], -2);
    di_note.Text := 'The frequency of ' + noteName + ' is: ' + FloatToStrF(frequencies[octave + 1, noteBaseOffset + 1], ffFixed, 8, 2) + ' Hz';
   end;
  end; 

end;

procedure tmainfo.oncreaex(const Sender: TObject);
var
  i, j, n: integer;  { Loop variables }
  powerFactor: real;
begin
  { Calculate the 12th root of 2 }
  root12of2 := exp(ln(2) / 12);

  { Calculate and fill the frequencies array }
  for i := 0 to NotesPerOctave - 1 do   { i = octave }
    for j := 0 to NotesPerOctave - 1 do  { j = note in the octave }
    begin
      { Calculate the number of semitone steps away from A4 (A4 = 440 Hz at octave 4) }
      n           := (i * NotesPerOctave + j) - (A4Octave * NotesPerOctave + A4Index);
      powerFactor := n / 12.0;
      frequencies[i + 1, j + 1] := A4Freq * exp(powerFactor * ln(2));  { Calculate frequency using exp and ln }
    end;
    
  caption := 'PianoNote v3 for ' + platformtext;
end;

procedure tmainfo.onfreqex(const Sender: TObject);
begin
  FindNearestNote(ed_frequency2.Value);
end;

end.

