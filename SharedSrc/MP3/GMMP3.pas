unit GMMP3;

interface

uses
  Windows, Classes;

function GMPlaySound(stream: TMemoryStream; flags: Cardinal): bool; overload;
function GMPlaySound(const fn: string; flags: Cardinal): bool; overload;
function ConvertMp3ToWav(InStream, OutStream: TStream): bool;

implementation

uses
  MMSystem, MMReg, MP3Utils, Math, SysUtils;

  // ======================================================================

const
  CResFileHeader: array [0 .. 7] of cardinal = ($00000000, $00000020, $0000FFFF, $0000FFFF, $00000000, $00000000, $00000000, $00000000);

type
  TResEntryHeader = packed record
    dwResSize: LongInt; // Size of the pure resource data
    dwHdrSize: LongInt; // Size of the header (incl. this)
  end;

  // Resource type as Z-terminated wide string
  // Resource name as Z-terminated wide string

const
  CResEntryTrailer: array [0 .. 3] of cardinal = ($00000000, $00000030, $00000000, $00000000);

procedure WriteResourceHeader(Stream: TStream; ResSize: LongInt; const ResName, ResType: string);
var
  reh: TResEntryHeader;
  rn, rt: WideString;
begin
  reh.dwResSize := ResSize;
  reh.dwHdrSize := 24 + (Length(ResName) + 1 + Length(ResType) + 1) * 2;
  rn := ResName;
  rt := ResType;

  Stream.WriteBuffer(CResFileHeader, 32);
  Stream.WriteBuffer(reh, 8);
  Stream.WriteBuffer(rt[1], 2 + 2 * Length(rt));
  Stream.WriteBuffer(rn[1], 2 + 2 * Length(rn));
  Stream.WriteBuffer(CResEntryTrailer, 16);
end;

// ======================================================================

const
  // FOURCC_RIFF = $46464952;   { 'RIFF' }

  FOURCC_WAVE = $45564157; { 'WAVE' }
  FOURCC_fmt = $20746D66; { 'fmt ' }
  FOURCC_fact = $74636166; { 'fact' }
  FOURCC_data = $61746164; { 'data' }

type
  TMp3RiffHeader = packed record
    fccRiff: FOURCC;
    dwFileSize: LongInt;
    fccWave: FOURCC;
    fccFmt: FOURCC;
    dwFmtSize: LongInt;
    mp3wfx: TMpegLayer3WaveFormat;
    fccFact: FOURCC;
    dwFactSize: LongInt;
    lSizeInSamples: LongInt;
    fccData: FOURCC;
    dwDataSize: LongInt;
  end;

procedure Mp3RiffHeaderFromInfo(var Riff: TMp3RiffHeader; const Header: TL3FHeader; Length: LongInt);
const
  CChannels: array [0 .. 3] of Word = (2, 2, 2, 1);
  CFlags: array [boolean, 0 .. 1] of cardinal = ((MPEGLAYER3_FLAG_PADDING_OFF, MPEGLAYER3_FLAG_PADDING_ON),
    (MPEGLAYER3_FLAG_PADDING_ISO, MPEGLAYER3_FLAG_PADDING_ISO));
  CSizeMismatch: array [boolean] of Integer = (1, 2);
begin
  Riff.fccRiff := FOURCC_RIFF;
  Riff.dwFileSize := Header.FileSize + SizeOf(TMp3RiffHeader);
  Riff.fccWave := FOURCC_WAVE;
  Riff.fccFmt := FOURCC_fmt;
  Riff.dwFmtSize := SizeOf(TMpegLayer3WaveFormat);
  Riff.mp3wfx.wfx.wFormatTag := WAVE_FORMAT_MPEGLAYER3;
  Riff.mp3wfx.wfx.nChannels := CChannels[Header.Mode];
  Riff.mp3wfx.wfx.nSamplesPerSec := l3f_header_freq_hz(Header);
  Riff.mp3wfx.wfx.nAvgBytesPerSec := 125 * l3f_header_rate_kbps(Header);
  Riff.mp3wfx.wfx.nBlockAlign := 1;
  Riff.mp3wfx.wfx.wBitsPerSample := 0;
  Riff.mp3wfx.wfx.cbSize := MPEGLAYER3_WFX_EXTRA_BYTES;
  Riff.mp3wfx.wID := MPEGLAYER3_ID_MPEG;
  Riff.mp3wfx.fdwFlags := CFlags[Header.XingHeader > 0, Header.Padding];
  Riff.mp3wfx.nBlockSize := Header.LengthInBytes;
  Riff.mp3wfx.nFramesPerBlock := 1;
  Riff.mp3wfx.nCodecDelay := 1105; // 1 + (Standard MDTC Filterbank) + (1 Granule)
  Riff.fccFact := FOURCC_fact;
  Riff.dwFactSize := 4;
  Riff.lSizeInSamples := (Header.TotalFrames - CSizeMismatch[Header.XingHeader > 0]) * Header.LengthInSamples - Riff.mp3wfx.nCodecDelay;
  Riff.fccData := FOURCC_data;
  Riff.dwDataSize := Header.FileSize;
end;

// ======================================================================

function ConvertMp3ToWav(InStream, OutStream: TStream): bool;
var
  Header: TL3FHeader;
  Length: LongInt;
  Mp3Hdr, OldHdr: TMp3RiffHeader;
begin
  Result := false;
  Length := Layer3EstimateLength(InStream, Header);
  if Length = 0 then Exit;

  Mp3RiffHeaderFromInfo(Mp3Hdr, Header, Length);
  if Header.FileOffset >= SizeOf(TMp3RiffHeader) - 12 then
  begin
    InStream.Position := 0;
    InStream.ReadBuffer(OldHdr, 20);
    if (OldHdr.fccRiff = FOURCC_RIFF)
        and (OldHdr.dwFileSize <= Header.FileOffset + Header.FileSize)
        and (OldHdr.fccWave = FOURCC_WAVE)
        and (OldHdr.fccFmt = FOURCC_fmt)
        and (OldHdr.dwFmtSize >= SizeOf(TMpegLayer3WaveFormat)) then
    begin
      // файл уже сконвертирован
      Exit;
    end;
  end;

  OutStream.WriteBuffer(Mp3Hdr, SizeOf(Mp3Hdr));
  InStream.Position := Header.FileOffset;
  OutStream.CopyFrom(InStream, Header.FileSize);

  Result := true;
end;

function GMPlaySound(stream: TMemoryStream; flags: Cardinal): bool;
var
  streamWav: TMemoryStream;
begin
  streamWav := TMemoryStream.Create();
  try
    if ConvertMp3ToWav(stream, streamWav) then
      Result := PlaySound(streamWav.Memory, 0, flags or SND_MEMORY)
    else
      Result := PlaySound(stream.Memory, 0, flags or SND_MEMORY)
  finally
    streamWav.Free();
  end;
end;

function GMPlaySound(const fn: string; flags: Cardinal): bool;
var
  fs: TFileStream;
  streamWav: TMemoryStream;
begin
  try
    fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    streamWav := TMemoryStream.Create();
    try
      if ConvertMp3ToWav(fs, streamWav) then
        Result := PlaySound(streamWav.Memory, 0, flags or SND_MEMORY)
      else
        Result := PlaySound(PChar(fn), 0, flags or SND_FILENAME)
    finally
      streamWav.Free();
      fs.Free();
    end;
  except
    Result := false;
  end;
end;

end.
