{$mode objfpc}

const
  MAP_WIDTH = 16;
  MAP_HEIGHT = 16;
  SCREEN_WIDTH = 320;
  SCREEN_WIDTH_MINUS_ONE = SCREEN_WIDTH - 1;
  SCREEN_HEIGHT = 168;
  SCREEN_HEIGHT_HALF = SCREEN_HEIGHT div 2;
  SCREEN_SEGMENT = $A000;
  BUFFER_SEGMENT = ($A0000 - 64000) shr 4;
  KEY_UP = 72;
  KEY_DOWN = 80;
  KEY_LEFT = 75;
  KEY_RIGHT = 77;
  KEY_ESCAPE = 1;
  F1 = 1 shl 16;
  F2 = 2 shl 16;
  F4 = 4 shl 16;
  F10 = 10 shl 16;
  F40 = 50 shl 16;
  FSH = SCREEN_HEIGHT_HALF shl 16;
  F0_1 = 6554; // FToFixed32(0.1);
  F0_6 = 43254; // FToFixed32(0.66);
  BACKGROUND_PALETTE: array[0..7] of Byte = (128, 129, 130, 131, 132, 133, 134, 135);
  Map: array[0..MAP_WIDTH * MAP_HEIGHT - 1] of Byte = (
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1,
    1, 0, 0, 5, 5, 5, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  );
  Texture: array[0..3] of Byte = (
    7, 4,
    4, 7
  );

type
  TFixed32 = LongInt;
  TFixed32Rec = packed record
    Frac: Word;
    Int: SmallInt;
  end;
  TPlayer = packed record
    X, Y, Angle: TFixed32;
  end;

var
  Player: TPlayer;
  SinTable,
  CosTable: array[0..360] of TFixed32;
  Keys: array[0..127] of Boolean;
  OldKeyVec: FarPointer;
  TextureScanDeltaCache: array[0..1023] of TFixed32;

function Fixed32Mul(const A, B: TFixed32): TFixed32; inline;
begin
  // Slowest mul, lost some precision
  Result := (A div 256) * (B div 256);
end;

function Fixed32MulHack(const A, B: TFixed32): TFixed32; inline;
begin
  // Fastest mul, B lost a lot of precision and must be positive
  Result := A * LongWord(B shr 16);
end;

function Fixed32MulHack2(const A, B: TFixed32): TFixed32; inline;
begin
  // 2nd fastest mul, lost some precision, positive * positive
  Result := LongWord(A shr 8) * LongWord(B shr 8);
end;

function Fixed32MulHack3(const A, B: TFixed32): TFixed32; inline;
begin
  // 3rd fastest mul, lost some precision, positive * (positive or negative)
  Result := LongWord(A shr 8) * (B div 256);
end;

function Fixed32Div(const A, B: TFixed32): TFixed32; inline;
begin
  // Slowest div, lost some precision
  Result := (A * 256) div (B div 256 or 1);
end;

function Fixed32DivHack(const A, B: TFixed32): TFixed32; inline;
begin
  // Fastest div, B lost a lot of precision and must be positive
  Result := A div LongWord(B shr 16);
end;

function Fixed32DivHack2(const A, B: TFixed32): TFixed32; inline;
begin
  // 2nd fastest div, lost some precision, positive / positive
  Result := LongWord(A shl 8) div LongWord(B shr 8 or 1);
end;

function ToFixed32(const V: SmallInt): TFixed32; inline;
begin
  TFixed32Rec(Result).Int := V;
  TFixed32Rec(Result).Frac := 0;
end;

function FToFixed32(const V: Single): TFixed32; inline;
begin
  Result := Round(V * (1 shl 16));
end;

function Fixed32ToF(const V: TFixed32): Single; inline;
begin
  Result := V / (1 shl 16);
end;

function Min(const V1, V2: SmallInt): SmallInt; assembler;
label
  EndP;
asm
  mov ax,V1
  mov dx,V2
  cmp ax,dx
  jle EndP
  mov ax,dx
EndP:
end;

function Max(const V1, V2: SmallInt): SmallInt; assembler;
label
  EndP;
asm
  mov ax,V1
  mov dx,V2
  cmp ax,dx
  jge EndP
  mov ax,dx
EndP:
end;

function Fixed32Sin(const V: TFixed32): TFixed32; inline;
var
  D: SmallInt;
  F: Word;
begin
  F := TFixed32Rec(V).Frac;
  D := TFixed32Rec(V).Int mod 360;
  if D < 0 then
    D := D + 360;
  Result := SinTable[D] + Fixed32MulHack(SinTable[D + 1] - SinTable[D], F);
end;

function Fixed32Cos(const V: TFixed32): TFixed32; inline;
var
  D: SmallInt;
  F: Word;
begin
  F := TFixed32Rec(V).Frac;
  D := TFixed32Rec(V).Int mod 360;
  if D < 0 then
    D := D + 360;
  Result := CosTable[D] + Fixed32MulHack(CosTable[D + 1] - CosTable[D], F);
end;

procedure BufferToScreen; assembler; nostackframe;
asm
  push ds
  mov cx,32000
  mov bx,BUFFER_SEGMENT
  mov ds,bx
  xor si,si
  mov bx,SCREEN_SEGMENT
  mov es,bx
  xor di,di
  rep movsw
  pop ds
end;

// Fast "unsafe" vertical line routine
procedure LineVert(const X, Y1, Y2: Word; const C: Byte); assembler;
label
  Loop, EndLoop;
asm
  // cx = Y2 - Y1
  mov cx,Y2
  sub cx,Y1
  // es = BUFFER_SEGMENT
  mov bx,BUFFER_SEGMENT
  mov es,bx
  // di = Y1 * SCREEN_WIDTH + X
  mov ax,Y1
  mov bx,SCREEN_WIDTH
  mul bx
  add ax,X
  mov di,ax
  // al = C
  mov al,C
Loop:
  mov byte [es:di],al
  dec cx
  jcxz EndLoop
  add di,SCREEN_WIDTH
  jmp Loop
EndLoop:
end;

// Fast "unsafe" horizonal line routine
procedure LineHorz(const X1, X2, Y: Word; const C: Byte); assembler;
label
  Loop, EndLoop;
asm
  // cx = X2 - X1
  mov cx,X2
  sub cx,X1
  shr cx,1
  // es = BUFFER_SEGMENT
  mov bx,BUFFER_SEGMENT
  mov es,bx
  // di = Y * SCREEN_WIDTH + X1
  mov ax,Y
  mov bx,SCREEN_WIDTH
  mul bx
  add ax,X1
  mov di,ax
  // ax = CC
  mov al,C
  mov ah,al
Loop:
  mov word [es:di],ax
  dec cx
  jcxz EndLoop
  add di,2
  jmp Loop
EndLoop:
end;

procedure SwitchMode13h; assembler; nostackframe;
asm
  mov ax,$13
  int $10
end;

procedure SwitchTextMode; assembler; nostackframe;
asm
  mov ax,$3
  int $10
end;

procedure WaitForVsync; assembler; nostackframe;
label
  L1, L2;
asm
  mov dx,$03DA
L1:
  in al,dx
  test al,$8
  jz L1
L2:
  in al,dx
  test al,$8
  jnz L2
end;

procedure DrawBackground;
var
  J: Word;
begin
  for J := 0 to SCREEN_HEIGHT - 1 do
    LineHorz(0, 320, J, BACKGROUND_PALETTE[(J + 1) div 25 mod 8]);
end;

procedure DrawStatusBar;
var
  J: Word;
begin
  for J := SCREEN_HEIGHT to 199 do
    LineHorz(0, 320, J, 2);
end;

procedure DrawWall(const X, H, TX: Word; const Wall: Word);
label
  StartL, EndL, StartDraw;
var
  I, L, F, Step: SmallInt;
  YInc, TY: TFixed32;
  P: Word;
begin
  YInc := TextureScanDeltaCache[H];
  TY := 0;
  L := SCREEN_HEIGHT_HALF - H;
  Step := Max(2, (Min(H, SCREEN_HEIGHT_HALF) shr 5));
  if L < 0 then
  begin
    TY := TY + (Fixed32MulHack(YInc, -(LongInt(L) shl 16)));
    L := 0;
  end;
  P := L * SCREEN_WIDTH + X;
  F := ((Min(SCREEN_HEIGHT_HALF + H, SCREEN_HEIGHT) - L) shr 1) - Step;
  YInc := Fixed32MulHack(YInc, LongInt(Step) shl 16);
  asm
    mov bx,BUFFER_SEGMENT
    mov es,bx
    xor si,si
    mov di,P
    // Draw loop
  StartDraw:
    // TY := TY + YInc;
    mov ax,word [TY]
    mov dx,word [TY+2]
    add ax,word [YInc]
    adc dx,word [YInc+2]
    mov word [TY],ax
    mov word [TY+2],dx

    // al := Texture[TX + (TFixed32Rec(TY).Int mod 2) shl 1] - S + B;
    xor ah,ah
    mov cl,2
    mov al,byte [TY+2]
    div cl
    shr ax,7
    mov bx,word [TX]
    add bx,ax
    mov al,byte [Texture + bx]
    mov cx,word [Wall]
    sub al,cl
    add al,ch

    mov ah,al
    mov cx,Step
  StartL:
    mov word [es:di],ax
    dec cx
    add di,SCREEN_WIDTH
    jcxz EndL
    jmp StartL
  EndL:
    inc si
    cmp si,F
    jne StartDraw
  end;
end;

procedure Raycast;
var
  RayAngleStart, RayAngleEnd, RayX, RayY, Distance,
  RayXStart, RayYStart, RayXEnd, RayYend, RayXStep, RayYStep,
  DeltaDistX, DeltaDistY, SideDistX, SideDistY, WallX, WallY: TFixed32;
  StepX, StepY,
  MapX, MapY: ShortInt;
  I, WallHeight: Word;
  Wall, IsShadow: Byte;
begin
  RayAngleStart := Player.Angle - F40;
  RayAngleEnd := Player.Angle + F40;
  RayXStart := Fixed32Cos(RayAngleStart);
  RayYStart := Fixed32Sin(RayAngleStart);
  RayXEnd := Fixed32Cos(RayAngleEnd);
  RayYEnd := Fixed32Sin(RayAngleEnd);
  RayXStep := Fixed32DivHack(RayXEnd - RayXStart, SCREEN_WIDTH shl 15);
  RayYStep := Fixed32DivHack(RayYEnd - RayYStart, SCREEN_WIDTH shl 15);
  RayX := RayXStart;
  RayY := RayYStart;
  for I := 0 to SCREEN_WIDTH shr 1 - 1 do
  begin
    MapX := TFixed32Rec(Player.X).Int;
    MapY := TFixed32Rec(Player.Y).Int;
    DeltaDistX := Abs(Fixed32Div((1 shl 16), RayX));
    DeltaDistY := Abs(Fixed32Div((1 shl 16), RayY));
    if RayX < 0 then
    begin
      StepX := -1;
      SideDistX := Fixed32MulHack2(Player.X - (LongInt(MapX) shl 16), DeltaDistX);
    end else
    begin
      StepX := 1;
      SideDistX := Fixed32MulHack2(((LongInt(MapX) + 1) shl 16) - Player.X, DeltaDistX);
    end;
    if RayY < 0 then
    begin
      StepY := -1;
      SideDistY := Fixed32MulHack2(Player.Y - (LongInt(MapY) shl 16), DeltaDistY);
    end else
    begin
      StepY := 1;
      SideDistY := Fixed32MulHack2(((LongInt(MapY) + 1) shl 16) - Player.Y, DeltaDistY);
    end;
    Wall := 0;
    while Wall = 0 do
    begin
      if SideDistX < SideDistY then
      begin
        SideDistX := SideDistX + DeltaDistX;
        MapX := MapX + StepX;
        IsShadow := 0;
      end else
      begin
        SideDistY := SideDistY + DeltaDistY;
        MapY := MapY + StepY;
        IsShadow := 1;
      end;
      Wall := Map[MapX + MapY * MAP_WIDTH];
      //if (MapX >= MAP_WIDTH) or (MapY >= MAP_HEIGHT) then
      //  Break;
    end;
    if Wall <> 0 then
    begin
      if IsShadow = 0 then
        Distance := Fixed32Div((LongInt(MapX) shl 16) - Player.X + Fixed32DivHack(LongInt(1 - StepX) shl 16, F2), RayX)
      else
        Distance := Fixed32Div((LongInt(MapY) shl 16) - Player.Y + Fixed32DivHack(LongInt(1 - StepY) shl 16, F2), RayY);
      WallX := Player.X + Fixed32MulHack3(Distance, RayX);
      WallY := Player.Y + Fixed32MulHack3(Distance, RayY);
      WallHeight := TFixed32Rec(Fixed32DivHack2(FSH, Distance)).Int;
      DrawWall(I shl 1, WallHeight, TFixed32Rec(Fixed32MulHack2(F4, WallX + WallY)).Int mod 2, (Wall - 1) shl 8 + IsShadow);
    end;
    RayX := RayX + RayXStep;
    RayY := RayY + RayYStep;
  end;
end;

procedure HandleInput;
var
  StepX, StepY, StepX5, StepY5: TFixed32;
  MapS1, MapS2, MapS3: Byte;
begin
  if Keys[KEY_UP] then
  begin
    StepX := Player.X + Fixed32MulHack3(F0_1, Fixed32Cos(Player.Angle));
    StepY := Player.Y + Fixed32MulHack3(F0_1, Fixed32Sin(Player.Angle));
    StepX5 := Player.X + Fixed32MulHack3(F0_6, Fixed32Cos(Player.Angle));
    StepY5 := Player.Y + Fixed32MulHack3(F0_6, Fixed32Sin(Player.Angle));
    MapS1 := Map[TFixed32Rec(StepX5).Int + TFixed32Rec(StepY5).Int * (MAP_WIDTH)];
    MapS2 := Map[TFixed32Rec(Player.X).Int + TFixed32Rec(StepY5).Int * (MAP_WIDTH)];
    MapS3 := Map[TFixed32Rec(StepX5).Int + TFixed32Rec(Player.Y).Int * (MAP_WIDTH)];
    if (MapS1 = 0) and (MapS2 = 0) and (MapS3 = 0) then
    begin
      Player.X := StepX;
      Player.Y := StepY;
    end else
    if MapS2 = 0 then
    begin
      Player.Y := StepY;
    end else
    if MapS3 = 0 then
    begin
      Player.X := StepX;
    end;
  end else
  if Keys[KEY_DOWN] then
  begin
    StepX := Player.X - Fixed32MulHack3(F0_1, Fixed32Cos(Player.Angle));
    StepY := Player.Y - Fixed32MulHack3(F0_1, Fixed32Sin(Player.Angle));
    StepX5 := Player.X - Fixed32MulHack3(F0_6, Fixed32Cos(Player.Angle));
    StepY5 := Player.Y - Fixed32MulHack3(F0_6, Fixed32Sin(Player.Angle));
    MapS1 := Map[TFixed32Rec(StepX5).Int + TFixed32Rec(StepY5).Int * (MAP_WIDTH)];
    MapS2 := Map[TFixed32Rec(Player.X).Int + TFixed32Rec(StepY5).Int * (MAP_WIDTH)];
    MapS3 := Map[TFixed32Rec(StepX5).Int + TFixed32Rec(Player.Y).Int * (MAP_WIDTH)];
    if (MapS1 = 0) and (MapS2 = 0) and (MapS3 = 0) then
    begin
      Player.X := StepX;
      Player.Y := StepY;
    end else
    if MapS2 = 0 then
    begin
      Player.Y := StepY;
    end else
    if MapS3 = 0 then
    begin
      Player.X := StepX;
    end;
  end;
  if Keys[KEY_LEFT] then
    Player.Angle := Player.Angle - F10
  else if Keys[KEY_RIGHT] then
    Player.Angle := Player.Angle + F10;
end;

procedure HandleMultiKey; interrupt;
var
  ScanCode: Word;
begin
  asm
    cli
    in al,$60
    xor ah,ah
    mov ScanCode,ax
    in al,$61
    or al,$82
    out $61,al
    and al,$7F
    out $61,al
    mov al,$20
    out $20,al
  end;
  if ScanCode < 128 then
    Keys[ScanCode] := True
  else
    Keys[ScanCode - 128] := False;
  asm
    sti
  end;
end;

procedure BackupKeyInterrupt; assembler; nostackframe;
asm
  push es
  mov ax,$3509
  int $21
  mov word [OldKeyVec],bx
  mov word [OldKeyVec + 2],es
  pop es
end;

procedure SetupKeyInterrupt; assembler; nostackframe;
asm
  push ds
  mov ax,$2509
  mov dx,HandleMultiKey
  mov bx,es
  mov ds,bx
  int $21
  pop ds
end;

procedure RestoreKeyInterrupt; assembler; nostackframe;
asm
  push ds
  mov ax,$2509
  mov dx,word [OldKeyVec]
  mov bx,word [OldKeyVec + 2]
  mov ds,bx
  int $21
  pop ds
end;

procedure SetupData;
var
  I: Word;
begin
  for I := 0 to 360 do
  begin
    SinTable[I] := FToFixed32(Sin(I * 0.0174532925) + 0.001);
    CosTable[I] := FToFixed32(Cos(I * 0.0174532925) + 0.001);
  end;
  for I := 0 to High(TextureScanDeltaCache) do
    TextureScanDeltaCache[I] := Fixed32DivHack2(F4, LongInt(I) shl 17)
end;

begin
  Player.X := FToFixed32(1.5);
  Player.Y := FToFixed32(2.5);
  BackupKeyInterrupt;
  SetupKeyInterrupt;
  SetupData;
  SwitchMode13h;
  repeat
    DrawBackground;
    HandleInput;
    Raycast;
    DrawStatusBar;
    BufferToScreen;
    WaitForVsync;
  until Keys[KEY_ESCAPE];
  SwitchTextMode;
  RestoreKeyInterrupt;
end.