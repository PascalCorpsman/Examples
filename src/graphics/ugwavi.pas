(******************************************************************************)
(* ugwavi.pas                                                      ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Class to create .avi files. Ported from C                    *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
(*
 * Orig Source: https://github.com/rolinh/libgwavi
 *
 * Orig Authors: 2008-2011, Michael Kohn
 *               2013, Robin Hahling
 *
 * C-Types: https://learn.microsoft.com/en-us/cpp/cpp/data-type-ranges?view=msvc-170
 *
 * Port to FPC by Corpsman
 *)
Unit ugwavi;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  TUintArray = Array Of UInt32;

  gwavi_audio_t = Record
    Channels: UInt32;
    bits: UInt32;
    samples_per_second: UInt32;
  End;

  p_gwavi_audio_t = ^gwavi_audio_t;

  gwavi_header_t = Record
    time_delay: UInt32; //* dwMicroSecPerFrame */
    data_rate: UInt32; //* dwMaxBytesPerSec */
    reserved: UInt32;
    flags: UInt32; //* dwFlags */
    number_of_frames: UInt32; //* dwTotalFrames */
    initial_frames: UInt32; //* dwInitialFrames */
    data_streams: UInt32; //* dwStreams */
    buffer_size: UInt32; //* dwSuggestedBufferSize */
    width: UInt32; //* dwWidth */
    height: UInt32; //* dwHeight */
    time_scale: UInt32;
    playback_data_rate: UInt32;
    starting_time: UInt32;
    data_length: UInt32;
  End;

  gwavi_stream_header_t = Record
    data_type: Array[0..4] Of Char; //* fccType */
    codec: Array[0..4] Of Char; //* fccHandler */
    flags: UInt32; //* dwFlags */
    priority: UInt32;
    initial_frames: UInt32; //* dwInitialFrames */
    time_scale: UInt32; //* dwScale */
    data_rate: UInt32; //* dwRate */
    start_time: UInt32; //* dwStart */
    data_length: UInt32; //* dwLength */
    buffer_size: UInt32; //* dwSuggestedBufferSize */
    video_quality: UInt32; //* dwQuality */
    (**
     * Value between 0-10000. If set to -1, drivers use default quality
     * value.
     *)
    audio_quality: int32;
    sample_size: UInt32; //* dwSampleSize */
  End;

  gwavi_stream_format_v_t = Record
    header_size: UInt32;
    width: UInt32;
    height: UInt32;
    num_planes: uint16;
    bits_per_pixel: uint16;
    compression_type: UInt32;
    image_size: UInt32;
    x_pels_per_meter: UInt32;
    y_pels_per_meter: UInt32;
    colors_used: UInt32;
    colors_important: UInt32;
    palette: Array Of UInt32;
    palette_count: UInt32;
  End;

  gwavi_stream_format_a_t = Record
    format_type: uint16;
    channels: UInt32;
    sample_rate: UInt32;
    bytes_per_second: UInt32;
    block_align: UInt32;
    bits_per_sample: UInt32;
    size: uint16;
  End;


  { tgwavi }

  tgwavi = Class
  private
    fFile: TFileStream;
    favi_header: gwavi_header_t;

    fstream_header_v: gwavi_stream_header_t;
    fstream_format_v: gwavi_stream_format_v_t;
    fstream_header_a: gwavi_stream_header_t;
    fstream_format_a: gwavi_stream_format_a_t;
    fmarker: int64;
    foffsets_ptr: int32;
    foffsets_len: int32;
    foffsets: TUintArray;
    foffset_count: int32;
    fLastError: String;
    Function write_avi_header_chunk(): integer;
    Function write_avi_header(Const avi_header: gwavi_header_t): integer;
    Function write_stream_header(Const stream_header: gwavi_stream_header_t): integer;
    Function write_stream_format_v(Const stream_format_v: gwavi_stream_format_v_t): integer;
    Function write_stream_format_a(Const stream_format_a: gwavi_stream_format_a_t): integer;
  public
    Property LastError: String read fLastError;
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    (*
     * 1. Datei Öffnen
     * fourcc = 'MJPG'
     *)
    Function Open(Const Filename: String; Width, Height: Integer; fourcc: String; fps: integer; audio: p_gwavi_audio_t): Boolean;
    (*
     * 2. [Optional] Einzelbilder anfügen, Achtung, dass scheint nur mit .jpg Dateien zu gehen ..
     *)
    Function Add_Frame(Stream: TStream): Boolean;
    (*
     * 3. [Optional] eine .wav Datei Anfügen, Achtung ungetestet
     *)
    Function Add_Audio(Stream: TStream): Boolean;
    (*
     * 4. Datei Schließen und damit gültig "abspeichern"
     *)
    Function Close(): Boolean;
  End;

Implementation

// ----------------------------------------- Helper functions ------------------

Const
  (*
   * list of fourccs from http://fourcc.org/codecs.php -> Link is dead
   *
   * here is a other link that holds lots of fourccs
   * https://www.libe.net/themen/FourCC-Codec-Codes.php#fourcc-codes
   *)
  valid_fourcc: Array Of String = (
    '3IV1', '3IV2', '8BPS',
    'AASC', 'ABYR', 'ADV1', 'ADVJ', 'AEMI', 'AFLC', 'AFLI', 'AJPG', 'AMPG', 'ANIM', 'AP41', 'ASLC',
    'ASV1', 'ASV2', 'ASVX', 'AUR2', 'AURA', 'AVC1', 'AVRN',
    'BA81', 'BINK', 'BLZ0', 'BT20', 'BTCV', 'BW10', 'BYR1', 'BYR2',
    'CC12', 'CDVC', 'CFCC', 'CGDI', 'CHAM', 'CJPG', 'CMYK', 'CPLA', 'CRAM', 'CSCD', 'CTRX', 'CVID',
    'CWLT', 'CXY1', 'CXY2', 'CYUV', 'CYUY',
    'D261', 'D263', 'DAVC', 'DCL1', 'DCL2', 'DCL3', 'DCL4', 'DCL5', 'DIV3', 'DIV4', 'DIV5', 'DIVX',
    'DM4V', 'DMB1', 'DMB2', 'DMK2', 'DSVD', 'DUCK', 'DV25', 'DV50', 'DVAN', 'DVCS', 'DVE2', 'DVH1',
    'DVHD', 'DVSD', 'DVSL', 'DVX1', 'DVX2', 'DVX3', 'DX50', 'DXGM', 'DXTC', 'DXTN',
    'EKQ0', 'ELK0', 'EM2V', 'ES07', 'ESCP', 'ETV1', 'ETV2', 'ETVC',
    'FFV1', 'FLJP', 'FMP4', 'FMVC', 'FPS1', 'FRWA', 'FRWD', 'FVF1',
    'GEOX', 'GJPG', 'GLZW', 'GPEG', 'GWLT',
    'H260', 'H261', 'H262', 'H263', 'H264', 'H265', 'H266', 'H267', 'H268', 'H269',
    'HDYC', 'HFYU', 'HMCR', 'HMRR',
    'I263', 'ICLB', 'IGOR', 'IJPG', 'ILVC', 'ILVR', 'IPDV', 'IR21', 'IRAW', 'ISME',
    'IV30', 'IV31', 'IV32', 'IV33', 'IV34', 'IV35', 'IV36', 'IV37', 'IV38', 'IV39', 'IV40', 'IV41',
    'IV41', 'IV43', 'IV44', 'IV45', 'IV46', 'IV47', 'IV48', 'IV49', 'IV50',
    'JBYR', 'JPEG', 'JPGL',
    'KMVC',
    'L261', 'L263', 'LBYR', 'LCMW', 'LCW2', 'LEAD', 'LGRY', 'LJ11', 'LJ22', 'LJ2K', 'LJ44', 'LJPG',
    'LMP2', 'LMP4', 'LSVC', 'LSVM', 'LSVX', 'LZO1',
    'M261', 'M263', 'M4CC', 'M4S2', 'MC12', 'MCAM', 'MJ2C', 'MJPG', 'MMES', 'MP2A', 'MP2T', 'MP2V',
    'MP42', 'MP43', 'MP4A', 'MP4S', 'MP4T', 'MP4V', 'MPEG', 'MPG4', 'MPGI', 'MR16', 'MRCA', 'MRLE',
    'MSVC', 'MSZH',
    'MTX1', 'MTX2', 'MTX3', 'MTX4', 'MTX5', 'MTX6', 'MTX7', 'MTX8', 'MTX9',
    'MVI1', 'MVI2', 'MWV1',
    'NAVI', 'NDSC', 'NDSM', 'NDSP', 'NDSS', 'NDXC', 'NDXH', 'NDXP', 'NDXS', 'NHVU', 'NTN1', 'NTN2',
    'NVDS', 'NVHS',
    'NVS0', 'NVS1', 'NVS2', 'NVS3', 'NVS4', 'NVS5',
    'NVT0', 'NVT1', 'NVT2', 'NVT3', 'NVT4', 'NVT5',
    'PDVC', 'PGVV', 'PHMO', 'PIM1', 'PIM2', 'PIMJ', 'PIXL', 'PJPG', 'PVEZ', 'PVMM', 'PVW2',
    'QPEG', 'QPEQ',
    'RGBT', 'RLE', 'RLE4', 'RLE8', 'RMP4', 'RPZA', 'RT21', 'RV20', 'RV30', 'RV40', 'S422', 'SAN3',
    'SDCC', 'SEDG', 'SFMC', 'SMP4', 'SMSC', 'SMSD', 'SMSV', 'SP40', 'SP44', 'SP54', 'SPIG', 'SQZ2',
    'STVA', 'STVB', 'STVC', 'STVX', 'STVY', 'SV10', 'SVQ1', 'SVQ3',
    'TLMS', 'TLST', 'TM20', 'TM2X', 'TMIC', 'TMOT', 'TR20', 'TSCC', 'TV10', 'TVJP', 'TVMJ', 'TY0N',
    'TY2C', 'TY2N',
    'UCOD', 'ULTI',
    'V210', 'V261', 'V655', 'VCR1', 'VCR2', 'VCR3', 'VCR4', 'VCR5', 'VCR6', 'VCR7', 'VCR8', 'VCR9',
    'VDCT', 'VDOM', 'VDTZ', 'VGPX', 'VIDS', 'VIFP', 'VIVO', 'VIXL', 'VLV1', 'VP30', 'VP31', 'VP40',
    'VP50', 'VP60', 'VP61', 'VP62', 'VP70', 'VP80', 'VQC1', 'VQC2', 'VQJC', 'VSSV', 'VUUU', 'VX1K',
    'VX2K', 'VXSP', 'VYU9', 'VYUY',
    'WBVC', 'WHAM', 'WINX', 'WJPG', 'WMV1', 'WMV2', 'WMV3', 'WMVA', 'WNV1', 'WVC1',
    'X263', 'X264', 'XLV0', 'XMPG', 'XVID',
    'XWV0', 'XWV1', 'XWV2', 'XWV3', 'XWV4', 'XWV5', 'XWV6', 'XWV7', 'XWV8', 'XWV9',
    'XXAN',
    'Y16', 'Y411', 'Y41P', 'Y444', 'Y8', 'YC12', 'YUV8', 'YUV9', 'YUVP', 'YUY2', 'YUYV', 'YV12', 'YV16',
    'YV92',
    'ZLIB ZMBV ZPEG ZYGO ZYYY'
    );
  EOF = -1;

Function write_chars_bin(Const Stream: TStream; Data: String; Len: integer
  ): Integer;
Var
  i: Integer;
  c: Char;
Begin
  result := -1; // Fehler
  For i := 1 To len Do Begin
    c := Data[i];
    stream.Write(c, sizeof(c));
  End;
  result := 0;
End;

Function fputc(Value: Integer; Const Stream: TStream): Integer;
Var
  b: UInt8;
Begin
  result := -1;
  b := value And 255;
  stream.Write(b, sizeof(b));
  result := 0;
End;

Function write_int(Const Stream: TStream; n: uint32): integer;
Var
  b: UInt8;
Begin
  result := -1;
  b := n And $FF;
  stream.Write(b, sizeof(b));
  b := (n Shr 8) And $FF;
  stream.Write(b, sizeof(b));
  b := (n Shr 16) And $FF;
  stream.Write(b, sizeof(b));
  b := (n Shr 24) And $FF;
  stream.Write(b, sizeof(b));
  result := 0;
End;

Function write_short(Const Stream: TStream; n: uint16): integer;
Var
  b: UInt8;
Begin
  result := -1;
  b := n And $FF;
  stream.Write(b, sizeof(b));
  b := (n Shr 8) And $FF;
  stream.Write(b, sizeof(b));
  result := 0;
End;

Function check_fourcc(Const fourcc: String): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(valid_fourcc) Do Begin
    If fourcc = valid_fourcc[i] Then Begin
      result := true;
      exit;
    End;
  End;
End;

Function write_index(Const Stream: TStream; Count: integer; Offsets: TUintArray
  ): integer;
Var
  marker, t: int64;
  offset: UInt32;
  i: integer;
Begin
  result := -1;
  offset := 4;

  If Not assigned(offsets) Then exit;
  //		return -1;

  If (write_chars_bin(Stream, 'idx1', 4) = -1) Then exit;
  //		(void)fprintf(stderr, "write_index: write_chars_bin) failed\n");
  //		return -1;
  //	}
  marker := Stream.Position;
  //	if ((marker = ftell(Stream)) == -1) {
  //		perror("write_index (ftell)");
  //		return -1;
  //	}
  If (write_int(Stream, 0) = -1) Then exit;
  //		goto write_int_failed;

  //	for (i = 0; i < count; i++) {
  For i := 0 To Count - 1 Do Begin
    If ((offsets[i] And $80000000) = 0) Then Begin
      write_chars_bin(Stream, '00dc', 4); // TODO: Passt das so ?
    End
    Else Begin
      write_chars_bin(Stream, '01wb', 4);
      offsets[i] := offsets[i] And $7FFFFFFF;
    End;
    If (write_int(Stream, $10) = -1) Then exit;
    //			goto write_int_failed;
    If (write_int(Stream, offset) = -1) Then exit;
    //			goto write_int_failed;
    If (write_int(Stream, offsets[i]) = -1) Then exit;
    //			goto write_int_failed;

    offset := offset + offsets[i] + 8;
  End;

  t := Stream.Position;
  //	if ((t = ftell(Stream)) = -1) {
  //		perror("write_index (ftell)");
  //		return -1;
  //	}
  Stream.Position := marker;
  //	if (fseek(Stream, marker, SEEK_SET) = -1) {
  //		perror("write_index (fseek)");
  //		return -1;
  //	}
  If (write_int(Stream, (t - marker - 4)) = -1) Then exit;
  //		goto write_int_failed;
  Stream.Position := t;
  //	if (fseek(Stream, t, SEEK_SET) = -1) {
  //		perror("write_index (fseek)");
  //		return -1;
  //	}

  result := 0;

  //write_int_failed:
  //	(void)fprintf(stderr, "write_index: write_int() failed\n");
  //	return -1;
End;


{ tgwavi }

Constructor tgwavi.Create;
Begin
  Inherited create;
  fFile := Nil;
End;

Destructor tgwavi.Destroy;
Begin
  If Assigned(fFile) Then Close();
End;

(**
 * This is the first function you should call when using gwavi library.
 * It allocates memory for a gwavi_t structure and returns it and takes care of
 * initializing the AVI header with the provided information.
 *
 * When you're done creating your AVI file, you should call gwavi_close()
 * function to free memory allocated for the gwavi_t structure and properly
 * close the output file.
 *
 * @param filename This is the name of the AVI file which will be generated by
 * this library.
 * @param width Width of a frame.
 * @param height Height of a frame.
 * @param fourcc FourCC representing the codec of the video encoded stream. a
 * FourCC is a sequence of four chars used to uniquely identify data formats.
 * For more information, you can visit www.fourcc.org.
 * @param fps Number of frames per second of your video. It needs to be > 0.
 * @param audio This parameter is optionnal. It is used for the audio track. If
 * you do not want to add an audio track to your AVI file, simply pass NULL for
 * this argument.
 *
 * @return Structure containing required information in order to create the AVI
 * file. If an error occured, NULL is returned.
 *)

Function tgwavi.Open(Const Filename: String; Width, Height: Integer;
  fourcc: String; fps: integer; audio: p_gwavi_audio_t): Boolean;
Begin
  result := false;
  fLastError := '';
  // Reset all internal Buffers in case the user calls multiple time open, ADD_*, close without freeing the instance
  foffset_count := 0;
  If assigned(fFile) Then ffile.free;
  ffile := Nil;
  If (Not check_fourcc(fourcc)) Then
    fLastError := fLastError + format('WARNING: given fourcc does not seem to be valid: %s' + LineEnding, [fourcc]);
  If (fps < 1) Then exit;
  fFile := TFileStream.Create(Filename, fmOpenWrite Or fmCreate);
  //	if ((out = fopen(filename, "wb+")) == NULL) {
  //		perror("gwavi_open: failed to open file for writing");
  //		return NULL;
  //	}

  //	if ((gwavi = (struct gwavi_t *)malloc(sizeof(struct gwavi_t))) == NULL) {
  //		(void)fprintf(stderr, "gwavi_open: could not allocate memoryi "
  //			      "for gwavi structure\n");
  //		return NULL;
  //	}
  //	memset(gwavi, 0, sizeof(struct gwavi_t));
  //
  //	gwavi->out = out;
  //
  //	/* set avi header */
  favi_header.time_delay := 1000000 Div fps;
  favi_header.data_rate := width * height * 3;
  favi_header.flags := $10;

  If assigned(audio) Then Begin
    favi_header.data_streams := 2;
  End
  Else Begin
    favi_header.data_streams := 1;
  End;
  //	/* this field gets updated when calling gwavi_close() */
  favi_header.number_of_frames := 0;
  favi_header.width := width;
  favi_header.height := height;
  favi_header.buffer_size := (width * height * 3);

  //	/* set stream header */
  fstream_header_v.data_type := 'vids';
  fstream_header_v.codec := copy(fourcc, 1, 4); // Sicherstellen, dass Codec max 4 byte hat ;)
  fstream_header_v.time_scale := 1;
  fstream_header_v.data_rate := fps;
  fstream_header_v.buffer_size := (width * height * 3);
  fstream_header_v.data_length := 0;

  //	/* set stream format */
  fstream_format_v.header_size := 40;
  fstream_format_v.width := width;
  fstream_format_v.height := height;
  fstream_format_v.num_planes := 1;
  fstream_format_v.bits_per_pixel := 24;
  fstream_format_v.compression_type :=
    (ord(fourcc[4]) Shl 24) +
    (ord(fourcc[3]) Shl 16) +
    (ord(fourcc[2]) Shl 8) +
    (ord(fourcc[1]));
  fstream_format_v.image_size := width * height * 3;
  fstream_format_v.colors_used := 0;
  fstream_format_v.colors_important := 0;

  fstream_format_v.palette := Nil;
  fstream_format_v.palette_count := 0;

  If assigned(audio) Then Begin
    //		/* set stream header */
    fstream_header_a.data_type := 'auds';
    fstream_header_a.codec[0] := chr(1);
    fstream_header_a.codec[1] := chr(0);
    fstream_header_a.codec[2] := chr(0);
    fstream_header_a.codec[3] := chr(0);
    fstream_header_a.time_scale := 1;
    fstream_header_a.data_rate := audio^.samples_per_second;
    fstream_header_a.buffer_size := audio^.channels * (audio^.bits Div 8) * audio^.samples_per_second;
    //		/* when set to -1, drivers use default quality value */
    fstream_header_a.audio_quality := -1;
    fstream_header_a.sample_size := (audio^.bits Div 8) * audio^.channels;

    //		/* set stream format */
    fstream_format_a.format_type := 1;
    fstream_format_a.channels := audio^.channels;
    fstream_format_a.sample_rate := audio^.samples_per_second;
    fstream_format_a.bytes_per_second := audio^.channels * (audio^.bits Div 8) * audio^.samples_per_second;
    fstream_format_a.block_align := audio^.channels * (audio^.bits Div 8);
    fstream_format_a.bits_per_sample := audio^.bits;
    fstream_format_a.size := 0;
  End;

  If (write_chars_bin(fFile, 'RIFF', 4) = -1) Then exit;
  //		goto write_chars_bin_failed;
  If (write_int(fFile, 0) = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_info: write_int() failed\n");
  //		return NULL;

  If (write_chars_bin(fFile, 'AVI ', 4) = -1) Then exit;
  //		goto write_chars_bin_failed;

  If (write_avi_header_chunk() = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_info: write_avi_header_chunk "
  //			      "failed\n");
  //		return NULL;
  //	}

  If (write_chars_bin(ffile, 'LIST', 4) = -1) Then exit;
  //		goto write_chars_bin_failed;
  fmarker := fFile.Position;
  //	if ((gwavi->marker = ftell(ffile)) = -1) {
  //		perror("gwavi_info (ftell)");
  //		return NULL;
  //	}
  If (write_int(ffile, 0) = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_info: write_int() failed\n");
  //		return NULL;
  //	}
  If (write_chars_bin(ffile, 'movi', 4) = -1) Then exit;
  //		goto write_chars_bin_failed;
  //
  Foffsets_len := 1024;
  foffsets := Nil;
  setlength(foffsets, foffsets_len);
  //If ((foffsets = (unsigned int * )malloc((size_t)gwavi - > offsets_len *
    //				      sizeof(unsigned int)))
    //			= NULL) {
    //		(void)fprintf(stderr, "gwavi_info: could not allocate memory "
    //			      "for gwavi offsets table\n");
    //		return NULL;
    //	}

  foffsets_ptr := 0;
  result := true;
  //	return gwavi;

  //write_chars_bin_failed:
  //	(void)fprintf(stderr, "gwavi_open: write_chars_bin() failed\n");
  //	return NULL;
End;

(**
 * This function allows you to add an encoded video frame to the AVI file.
 *
 * @param gwavi Main gwavi structure initialized with gwavi_open()-
 * @param buffer Video buffer size.
 * @param len Video buffer length.
 *
 * @return 0 on success, -1 on error.
 *)

Function tgwavi.Add_Frame(Stream: TStream): Boolean;
Var
  maxi_pad, len: int64;
  t: Integer;
Begin
  result := false;
  If Not assigned(fFile) Then exit; // Fehler keine Zieldatei geöffnet !
  If Not assigned(Stream) Then exit;
  len := Stream.Size - Stream.Position;


  //size_t maxi_pad;  /* if your frame is raggin, give it some paddin' */
  //	size_t t;
  //
  //	if (!gwavi || !buffer) {
  //		(void)fputs("gwavi and/or buffer argument cannot be NULL",
  //			    stderr);
  //		return -1;
  //	}
  //	if (len < 256)
  //		(void)fprintf(stderr, "WARNING: specified buffer len seems "
  //			      "rather small: %d. Are you sure about this?\n",
  //			      (int)len);

  Foffset_count := Foffset_count + 1;
  fstream_header_v.data_length := fstream_header_v.data_length + 1;

  maxi_pad := len Mod 4;
  If (maxi_pad > 0) Then maxi_pad := 4 - maxi_pad;

  If (foffset_count >= foffsets_len) Then Begin
    foffsets_len := foffsets_len + 1024;
    SetLength(foffsets, foffsets_len);
    //		gwavi->offsets = (unsigned int *)realloc(gwavi->offsets,
    //					(size_t)gwavi->offsets_len *
    //					sizeof(unsigned int));
  End;

  foffsets[foffsets_ptr] := (len + maxi_pad);
  foffsets_ptr := foffsets_ptr + 1;

  If (write_chars_bin(ffile, '00dc', 4) = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_add_frame: write_chars_bin() "
  //			      "failed\n");
  //		return -1;
  //	}
  If (write_int(ffile, (len + maxi_pad)) = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_add_frame: write_int() failed\n");
  //		return -1;
  //	}
  If fFile.CopyFrom(stream, len) <> len Then exit;

  //	if ((t = fwrite(buffer, 1, len, gwavi->out)) != len) {
  //		(void)fprintf(stderr, "gwavi_add_frame: fwrite() failed\n");
  //		return -1;
  //	}
  //
   //for (t = 0; t < maxi_pad; t++)
  For t := 0 To maxi_pad - 1 Do Begin
    If (fputc(0, fFile) = EOF) Then exit;
    //			(void)fprintf(stderr, "gwavi_add_frame: fputc() failed\n");
    //			return -1;
    //		}
  End;
  result := true;
End;

(**
 * This function allows you to add the audio track to your AVI file.
 *
 * @param gwavi Main gwavi structure initialized with gwavi_open()-
 * @param buffer Audio buffer size.
 * @param len Audio buffer length.
 *
 * @return 0 on success, -1 on error.
 *)

Function tgwavi.Add_Audio(Stream: TStream): Boolean;
Var
  maxi_pad, len: int64;
  t: Integer;
Begin
  result := false;
  If Not assigned(fFile) Then exit; // Fehler keine Zieldatei geöffnet !
  If Not assigned(Stream) Then exit;
  len := Stream.Size - Stream.Position;

  //  size_t maxi_pad;  /* in case audio bleeds over the 4 byte boundary  */
  //size_t t;
  //
  //if (!gwavi || !buffer) {
  //	(void)fputs("gwavi and/or buffer argument cannot be NULL",
  //		    stderr);
  //	return -1;
  //}
  //
  foffset_count := foffset_count + 1;

  maxi_pad := len Mod 4;
  If (maxi_pad > 0) Then maxi_pad := 4 - maxi_pad;

  If (foffset_count >= foffsets_len) Then Begin
    foffsets_len := foffsets_len + 1024;
    SetLength(foffsets, foffsets_len);
    //		gwavi->offsets = (unsigned int *)realloc(gwavi->offsets,
    //					(size_t)gwavi->offsets_len *
    //					sizeof(unsigned int));
  End;

  foffsets[foffsets_ptr] := ((len + maxi_pad) Or $80000000);
  foffsets_ptr := foffsets_ptr + 1;
  If (write_chars_bin(ffile, '01wb', 4) = -1) Then exit;
  //	(void)fprintf(stderr, "gwavi_add_audio: write_chars_bin() "
  //		      "failed\n");
  //	return -1;
  //}
  If (write_int(ffile, (len + maxi_pad)) = -1) Then exit;
  //	(void)fprintf(stderr, "gwavi_add_audio: write_int() failed\n");
  //	return -1;
  //}

  If fFile.CopyFrom(stream, len) <> len Then exit;
  //if ((t = fwrite(buffer, 1, len, ffile)) != len ) {
  //	(void)fprintf(stderr, "gwavi_add_audio: fwrite() failed\n");
  //	return -1;
  //}

  //for (t = 0; t < maxi_pad; t++)
  //	if (fputc(0,ffile) = EOF) {
  //		(void)fprintf(stderr, "gwavi_add_audio: fputc() failed\n");
  //		return -1;
  //	}
  For t := 0 To maxi_pad - 1 Do Begin
    If (fputc(0, fFile) = EOF) Then exit;
    //			(void)fprintf(stderr, "gwavi_add_frame: fputc() failed\n");
    //			return -1;
    //		}
  End;

  fstream_header_a.data_length := fstream_header_a.data_length + (len + maxi_pad);

  result := true;
End;

(**
 * This function should be called when the program is done adding video and/or
 * audio frames to the AVI file. It frees memory allocated for gwavi_open() for
 * the main gwavi_t structure. It also properly closes the output file.
 *
 * @param gwavi Main gwavi structure initialized with gwavi_open()-
 *
 * @return 0 on success, -1 on error.
 *)

Function tgwavi.Close: Boolean;
Var
  t: int64;
Begin
  result := false;
  //  long t;
  //
  //	if (!gwavi) {
  //		(void)fputs("gwavi argument cannot be NULL", stderr);
  //		return -1;
  //	}

  t := fFile.Position;
  //	if ((t = ftell(gwavi->out)) == -1)
  //		goto ftell_failed;
  fFile.Position := fmarker;
  //	if (fseek(gwavi->out, gwavi->marker, SEEK_SET) == -1)
  //		goto fseek_failed;
  If (write_int(ffile, (t - fmarker - 4)) = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_close: write_int() failed\n");
  //		return -1;
  //	}
  fFile.Position := t;
  //	if (fseek(ffile,t,SEEK_SET) = -1)
  //		goto fseek_failed;

  If (write_index(ffile, foffset_count, foffsets) = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_close: write_index() failed\n");
  //		return -1;
  //	}

  setlength(foffsets, 0);
  // TODO: Reset der ganzen Zähler
  //	free(gwavi->offsets);

  //	/* reset some avi header fields */
  favi_header.number_of_frames := fstream_header_v.data_length;

  t := fFile.Position;
  //	if ((t = ftell(ffile)) = -1)
  //		goto ftell_failed;
  fFile.Position := 12;
  //	if (fseek(ffile, 12, SEEK_SET) = -1)
  //		goto fseek_failed;
  If (write_avi_header_chunk() = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_close: write_avi_header_chunk() "
  //			      "failed\n");
  //		return -1;
  //	}
  //fFile.Position := t;
  //if (fseek(ffile, t, SEEK_SET) = -1)
 //		goto fseek_failed;

 //	if ((t = ftell(ffile)) = -1)
 //		goto ftell_failed;
  fFile.Position := 4;
  //	if (fseek(ffile, 4, SEEK_SET) = -1)
  //		goto fseek_failed;
  If (write_int(ffile, (t - 8)) = -1) Then exit;
  //		(void)fprintf(stderr, "gwavi_close: write_int() failed\n");
  //		return -1;
  //	}
  fFile.Position := t;
  //	if (fseek(ffile, t, SEEK_SET) = -1)
  //		goto fseek_failed;

  If assigned(fstream_format_v.palette) Then Begin
    //		free(gwavi->stream_format_v.palette);
    SetLength(fstream_format_v.palette, 0);
  End;
  fFile.Free;
  fFile := Nil;
  //	if (fclose(ffile) = EOF) {
  //		perror("gwavi_close (fclose)");
  //		return -1;
  //	}
  //	free(gwavi);
  //
  result := true;
  //
  //ftell_failed:
  //	perror("gwavi_close: (ftell)");
  //	return -1;
  //
  //fseek_failed:
  //	perror("gwavi_close (fseek)");
  //	return -1;
End;

Function tgwavi.write_avi_header_chunk: integer;
Var
  sub_marker, marker, t: int64;
Begin
  result := -1;

  //  long , t;
  //  long sub_marker;
  //  FILE *out = gwavi->out;
  //
  If (write_chars_bin(fFile, 'LIST', 4) = -1) Then exit;
  //  	goto write_chars_bin_failed;
  marker := fFile.Position;
  //if ((marker = ftell(fFile)) == -1)
//  	goto ftell_failed;

  If (write_int(fFile, 0) = -1) Then exit;
  //  	goto write_int_failed;
  If (write_chars_bin(fFile, 'hdrl', 4) = -1) Then exit;
  //  	goto write_chars_bin_failed;
  If (write_avi_header(favi_header) = -1) Then exit;
  //  	(void)fprintf(stderr, "write_avi_header_chunk: "
  //  		      "write_avi_header() failed\n");
  //  	return -1;
  //  }

  If (write_chars_bin(fFile, 'LIST', 4) = -1) Then exit;
  //  	goto write_chars_bin_failed;
  sub_marker := fFile.Position;
  //  if ((sub_marker = ftell(fFile)) == -1)
  //  	goto ftell_failed;
  If (write_int(fFile, 0) = -1) Then exit;
  //  	goto write_int_failed;
  If (write_chars_bin(fFile, 'strl', 4) = -1) Then exit;
  //  	goto write_chars_bin_failed;
  If (write_stream_header(fstream_header_v) = -1) Then exit;
  //  	(void)fprintf(stderr, "write_avi_header_chunk: "
  //  		      "write_stream_header failed\n");
  //  	return -1;
  //  }

  If (write_stream_format_v(fstream_format_v) = -1) Then exit;
  //  	(void)fprintf(stderr, "write_avi_header_chunk: "
  //  		      "write_stream_format_v failed\n");
  //  	return -1;
  //  }

  t := fFile.Position;
  //  if ((t = ftell(fFile)) == -1)
  //  	goto ftell_failed;

  fFile.Position := sub_marker;
  //  if (fseek(fFile, sub_marker, SEEK_SET) == -1)
  //  	goto fseek_failed;
  If (write_int(fFile, (t - sub_marker - 4)) = -1) Then exit;
  //  	goto write_int_failed;
  fFile.Position := t;
  //  if (fseek(fFile, t, SEEK_SET) == -1)
  //  	goto fseek_failed;

  If (favi_header.data_streams = 2) Then Begin
    If (write_chars_bin(fFile, 'LIST', 4) = -1) Then exit;
    //  		goto write_chars_bin_failed;
    sub_marker := fFile.Position;
    //  	if ((sub_marker = ftell(fFile)) == -1)
    //  		goto ftell_failed;
    If (write_int(fFile, 0) = -1) Then exit;
    //  		goto write_int_failed;
    If (write_chars_bin(fFile, 'strl', 4) = -1) Then exit;
    //  		goto write_chars_bin_failed;
    If (write_stream_header(fstream_header_a) = -1) Then exit;
    //  		(void)fprintf(stderr, "write_avi_header_chunk: "
    //  			      "write_stream_header failed\n");
    //  		return -1;
    //  	}

    If (write_stream_format_a(fstream_format_a) = -1) Then exit;
    //  		(void)fprintf(stderr, "write_avi_header_chunk: "
    //  			      "write_stream_format_a failed\n");
    //  		return -1;
    //  	}

    t := fFile.Position;
    //  	if ((t = ftell(fFile)) == -1)
    //  		goto ftell_failed;
    fFile.Position := sub_marker;
    //  	if (fseek(fFile, sub_marker, SEEK_SET) == -1)
    //  		goto fseek_failed;
    If (write_int(fFile, (t - sub_marker - 4)) = -1) Then exit;
    //  		goto write_int_failed;
    fFile.Position := t;
    //  	if (fseek(fFile, t, SEEK_SET) == -1)
    //  		goto fseek_failed;
  End;

  t := fFile.Position;
  //  if ((t = ftell(fFile)) == -1)
  //  	goto ftell_failed;
  fFile.Position := marker;
  //  if (fseek(fFile, marker, SEEK_SET) == -1)
  //  	goto fseek_failed;
  If (write_int(fFile, (t - marker - 4)) = -1) Then exit;
  //  	goto write_int_failed;
  fFile.Position := t;
  //  if (fseek(fFile, t, SEEK_SET) == -1)
  //  	goto fseek_failed;


  //ftell_failed:
  //  perror("write_avi_header_chunk (ftell)");
  //  return -1;
  //
  //fseek_failed:
  //  perror("write_avi_header_chunk (fseek)");
  //  return -1;
  //
  //write_int_failed:
  //  (void)fprintf(stderr, "write_avi_header_chunk: write_int() failed\n");
  //  return -1;
  //
  //write_chars_bin_failed:
  //  (void)fprintf(stderr, "write_avi_header_chunk: write_chars_bin() failed\n");
  //  return -1;

  result := 0;
End;

Function tgwavi.write_avi_header(Const avi_header: gwavi_header_t): integer;
Var
  Marker, t: Int64;
Begin
  result := -1;

  If (write_chars_bin(fFile, 'avih', 4) = -1) Then exit;
  //		(void)fprintf(stderr, "write_avi_header: write_chars_bin() "
  //			      "failed\n");
  //		return -1;
  //	}
  marker := fFile.Position;
  //	if ((marker = ftell(fFile)) == -1) {
  //		perror("write_avi_header (ftell)");
  //		return -1;
  //}
  If (write_int(fFile, 0) = -1) Then exit;
  //		goto write_int_failed;

  If (write_int(fFile, avi_header.time_delay) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.data_rate) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.reserved) = -1) Then exit;
  //		goto write_int_failed;
  //	/* dwFlags */
  If (write_int(fFile, avi_header.flags) = -1) Then exit;
  //		goto write_int_failed;
  //	/* dwTotalFrames */
  If (write_int(fFile, avi_header.number_of_frames) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.initial_frames) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.data_streams) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.buffer_size) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.width) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.height) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.time_scale) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.playback_data_rate) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.starting_time) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(fFile, avi_header.data_length) = -1) Then exit;
  //		goto write_int_failed;

  t := fFile.Position;
  //	if ((t = ftell(fFile)) == -1) {
  //		perror("write_avi_header (ftell)");
  //		return -1;
  //	}
  fFile.Position := Marker;
  //	if (fseek(fFile, marker, SEEK_SET) == -1) {
  //		perror("write_avi_header (fseek)");
  //		return -1;
  //	}
  If (write_int(fFile, (t - marker - 4)) = -1) Then exit;
  //		goto write_int_failed;
  fFile.Position := t;
  //	if (fseek(fFile, t, SEEK_SET) == -1) {
  //		perror("write_avi_header (fseek)");
  //		return -1;
  //	}

  result := 0;

  //write_int_failed:
  //	(void)fprintf(stderr, "write_avi_header: write_int() failed\n");
  //	return -1;
End;

Function tgwavi.write_stream_header(Const stream_header: gwavi_stream_header_t
  ): integer;
Var
  marker, t: int64;
Begin
  result := -1;
  //  long marker, t;

  If (write_chars_bin(ffile, 'strh', 4) = -1) Then exit;
  //  		goto write_chars_bin_failed;
  marker := fFile.Position;
  //  	if ((marker = ftell(ffile)) = -1) {
  //  		perror("write_stream_header (ftell)");
  //  		return -1;
  //  	}
  If (write_int(ffile, 0) = -1) Then exit;
  //  		goto write_int_failed;

  If (write_chars_bin(ffile, stream_header.data_type, 4) = -1) Then exit;
  //  		goto write_chars_bin_failed;
  If (write_chars_bin(ffile, stream_header.codec, 4) = -1) Then exit;
  //  		goto write_chars_bin_failed;
  If (write_int(ffile, stream_header.flags) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.priority) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.initial_frames) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.time_scale) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.data_rate) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.start_time) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.data_length) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.buffer_size) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.video_quality) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, stream_header.sample_size) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, 0) = -1) Then exit;
  //  		goto write_int_failed;
  If (write_int(ffile, 0) = -1) Then exit;
  //  		goto write_int_failed;
  t := fFile.Position;
  //  	if ((t = ftell(ffile)) = -1) {
  //  		perror("write_stream_header (ftell)");
  //  		return -1;
  //  	}
  fFile.Position := marker;
  //  	if (fseek(ffile, marker, SEEK_SET) = -1) {
  //  		perror("write_stream_header (fseek)");
  //  		return -1;
  //  	}
  write_int(ffile, (t - marker - 4));
  fFile.Position := t;
  //  	if (fseek(ffile, t, SEEK_SET) = -1){
  //  		perror("write_stream_header (fseek)");
  //  		return -1;
  //  	}
  //
  result := 0;
  //
  //  write_int_failed:
  //  	(void)fprintf(stderr, "write_stream_header: write_int() failed\n");
  //  	return -1;
  //
  //  write_chars_bin_failed:
  //  	(void)fprintf(stderr, "write_stream_header: write_chars_bin() failed\n");
  //  	return -1;
End;

Function tgwavi.write_stream_format_v(
  Const stream_format_v: gwavi_stream_format_v_t): integer;
Var
  marker, t: int64;
  i: Integer;
Begin
  result := -1;
  // 	long marker,t;
  //	unsigned int i;

  If (write_chars_bin(ffile, 'strf', 4) = -1) Then exit;
  //		(void)fprintf(stderr, "write_stream_format_v: write_chars_bin()"
  //			      " failed\n");
  //		return -1;
  //	}
  marker := fFile.Position;
  //	if ((marker = ftell(ffile)) = -1) {
  //		perror("write_stream_format_v (ftell)");
  //		return -1;
  //	}
  If (write_int(ffile, 0) = -1) Then exit;
  //		goto write_int_failed;

  If (write_int(ffile, stream_format_v.header_size) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_v.width) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_v.height) = -1) Then exit;
  //		goto write_int_failed;
  If (write_short(ffile, stream_format_v.num_planes) = -1) Then exit;
  //		(void)fprintf(stderr, "write_stream_format_v: write_short() "
  //			      "failed\n");
  //		return -1;
  //	}
  If (write_short(ffile, stream_format_v.bits_per_pixel) = -1) Then exit;
  //		(void)fprintf(stderr, "write_stream_format_v: write_short() "
  //			      "failed\n");
  //		return -1;
  //	}
  If (write_int(ffile, stream_format_v.compression_type) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_v.image_size) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_v.x_pels_per_meter) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_v.y_pels_per_meter) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_v.colors_used) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_v.colors_important) = -1) Then exit;
  //		goto write_int_failed;

  If (stream_format_v.colors_used <> 0) Then Begin
    //		for (i = 0; i < stream_format_v.colors_used; i++) {
    For i := 0 To stream_format_v.colors_used - 1 Do Begin
      If (fputc(stream_format_v.palette[i] And 255, ffile) = EOF) Then exit;
      //
      //				goto fputc_failed;
      If (fputc((stream_format_v.palette[i] Shr 8) And 255, ffile) = EOF) Then exit;
      //
      //				goto fputc_failed;
      If (fputc((stream_format_v.palette[i] Shr 16) And 255, ffile) = EOF) Then exit;
      //
      //				goto fputc_failed;
      If (fputc(0, ffile) = EOF) Then exit;
      //				goto fputc_failed;
    End;
  End;
  t := fFile.Position;
  //	if ((t = ftell(ffile)) = -1) {
  //		perror("write_stream_format_v (ftell)");
  //		return -1;
  //	}
  fFile.Position := marker;
  //	if (fseek(ffile,marker,SEEK_SET) = -1) {
  //		perror("write_stream_format_v (fseek)");
  //		return -1;
  //	}
  If (write_int(ffile, (t - marker - 4)) = -1) Then exit;
  //		goto write_int_failed;
  fFile.Position := t;
  //	if (fseek(ffile, t, SEEK_SET) = -1) {
  //		perror("write_stream_format_v (fseek)");
  //		return -1;
  //	}
  //
  result := 0;
  //
  //write_int_failed:
  //	(void)fprintf(stderr, "write_stream_format_v: write_int() failed\n");
  //	return -1;
  //
  //fputc_failed:
  //	(void)fprintf(stderr, "write_stream_format_v: fputc() failed\n");
  //	return -1;
End;

Function tgwavi.write_stream_format_a(
  Const stream_format_a: gwavi_stream_format_a_t): integer;
Var
  marker, t: int64;
Begin
  result := -1;
  //  long marker, t;
  //
  If (write_chars_bin(ffile, 'strf', 4) = -1) Then exit;
  //		(void)fprintf(stderr, "write_stream_format_a: write_chars_bin()"
  //			      " failed\n");
  //		return -1;
  //}
  marker := fFile.Position;
  //	if ((marker = ftell(ffile)) = -1) {
//		perror("write_stream_format_a (ftell)");
//		return -1;
//	}
  If (write_int(ffile, 0) = -1) Then exit;
  //		goto write_int_failed;

  If (write_short(ffile, stream_format_a.format_type) = -1) Then exit;
  //		goto write_short_failed;
  If (write_short(ffile, stream_format_a.channels) = -1) Then exit;
  //		goto write_short_failed;
  If (write_int(ffile, stream_format_a.sample_rate) = -1) Then exit;
  //		goto write_int_failed;
  If (write_int(ffile, stream_format_a.bytes_per_second) = -1) Then exit;
  //		goto write_int_failed;
  If (write_short(ffile, stream_format_a.block_align) = -1) Then exit;
  //		goto write_short_failed;
  If (write_short(ffile, stream_format_a.bits_per_sample) = -1) Then exit;
  //		goto write_short_failed;
  If (write_short(ffile, stream_format_a.size) = -1) Then exit;
  //		goto write_short_failed;
  //
  t := fFile.Position;
  //if ((t = ftell(ffile)) = -1) {
 //		perror("write_stream_format_a (ftell)");
 //		return -1;
 //	}
  fFile.Position := marker;
  //	if (fseek(ffile, marker, SEEK_SET) = -1) {
  //		perror("write_stream_format_a (fseek)");
  //		return -1;
  //	}
  If (write_int(ffile, (t - marker - 4)) = -1) Then exit;
  //		goto write_int_failed;
  fFile.Position := t;
  //	if (fseek(ffile, t, SEEK_SET) = -1) {
  //		perror("write_stream_format_a (fseek)");
  //		return -1;
  //	}

  result := 0;

  //write_int_failed:
  //	(void)fprintf(stderr, "write_stream_format_a: write_int() failed\n");
  //	return -1;
  //
  //write_short_failed:
  //	(void)fprintf(stderr, "write_stream_format_a: write_short() failed\n");
  //	return -1;
End;


End.

