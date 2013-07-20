{******************************************************************}
{                                                                  }
{ PC/SC Interface component                                        }
{ Helps you access a cardreader through Microsofts SmartCard API   }
{                                                                  }
{ The Original Code is PCSCConnector.pas                           }
{                                                                  }
{ The Initial Developer of the Original Code is                    }
{ Norbert Huettisch (nobbi(at)nobbi.com)                           }
{                                                                  }
{ Any suggestions and improvements to the code are appreciated     }
{                                                                  }
{ This Code uses a modified   SCardErr.pas (included)              }
{ This Code uses a modified   WinSCard.pas (included)              }
{ This code uses the original WinSmCrd.pas (included)              }
{                                                                  }
{ All originally made by Chris Dickerson (chrisd(at)tsc.com),      }
{ available as 'Interface units for the Microsoft Smart Card API'  }
{ at the Project JEDI Homepage http://www.delphi-jedi.org          }
{                                                                  }
{ Version info:                                                    }
{ 021230 - initial version                                         }
{ 030101 - routed errors from 'init' to the OnError event          }
{                                                                  }
{                                                                  }
{******************************************************************}
{                                                                  }
{ The contents of this file are subject to the                     }
{                                                                  }
{       Mozilla Public License Version 1.1 (the "License")         }
{                                                                  }
{ You may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at                          }
{ http://www.mozilla.org/MPL/                                      }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit PCSCConnector;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils,
  SCardErr, WinSCard, WinSmCrd;

type
  TErrSource         = (esInit, esConnect, esGetStatus, esTransmit);
  TNeededPIN         = (npPIN1, npPIN2, npPUK1, npPUK2);
  TDelimiters        = set of Char;

  TPCSCErrorEvent    = procedure(Sender: TObject; ErrSource: TErrSource; ErrCode: cardinal) of object;
  TPCSCPinEvent      = procedure(Sender: TObject; NeedPIN: TNeededPIN) of object;

const
  MAXAPDULENGTH      = 260; // CLA + INS + P1..3 + 255Bytes
  NOREADERSELECTED   = -1;
  SCARD_PCI_T0       : SCARD_IO_REQUEST = (dwProtocol:1; dbPciLength:8);
  SCARD_PCI_T1       : SCARD_IO_REQUEST = (dwProtocol:2; dbPciLength:8);
  SCARD_PROTOCOL_T0  = $00000001;
  SCARD_PROTOCOL_T1  = $00000002;
  SCARD_PROTOCOL_RAW = $00010000;
  SCARD_PROTOCOL_UNK = $00000000;

  WM_CARDSTATE     = WM_USER + 42;

  GSMStatusOK           = $9000;
  GSMStatusMemoryError  = $9240;
  GSMStatusNoEFSelected = $9400;
  GSMStatusOutOfRange   = $9402;
  GSMStatusNotFound     = $9404;
  GSMStatusFCDoNotMatch = $9408;
  GSMStatusCHVNeeded    = $9802;
  GSMStatusAuthFailed   = $9804;
  GSMStatusAuthFailedBl = $9840;
  GSMStatusTechProblem  = $6F00;
  GSMStatusResponseData = $9F;

  GSMFileTypeRFU = 0;
  GSMFileTypeMF  = 1;
  GSMFileTypeDF  = 2;
  GSMFileTypeEF  = 4;

  GSMEfTransp    = 0;
  GSMEfLinFixed  = 1;
  GSMEfCyclic    = 3;

type
  TPCSCConnector = class(TComponent)

  protected
    FContext            : cardinal;
    FCardHandle         : integer;
    FConnected          : boolean;
    FNumReaders         : integer;
    FUseReaderNum       : integer;
    FReaderList         : TStringlist;
    FAttrProtocol       : integer;
    FAttrICCType        : string;
    FAttrCardATR        : string;
    FAttrVendorName     : string;
    FAttrVendorSerial   : string;
    FGSMCurrentFile     : string;
    FGSMFileInfo        : string;
    FGSMDirInfo         : string;
    FGSMVoltage30       : boolean;
    FGSMVoltage18       : boolean;

    FOnReaderWaiting    : TNotifyEvent;
    FOnReaderListChange : TNotifyEvent;
    FOnCardInserted     : TNotifyEvent;
    FOnCardActive       : TNotifyEvent;
    FOnCardRemoved      : TNotifyEvent;
    FOnCardInvalid      : TNotifyEvent;
    FOnError            : TPCSCErrorEvent;
    FOnCHVNeeded        : TPCSCPinEvent;

    procedure SetReaderNum(Value: integer);
    procedure MessageWndProc(var Msg: TMessage);
    function  ConnectSelectedReader: boolean;
    procedure ProcessReaderState(const OldState,NewState: cardinal);
    procedure GetReaderAttributes;
    procedure GetCardAttributes;
    procedure ClearReaderAttributes;
    procedure ClearCardAttributes;
    function  IsReaderOpen: boolean;
    function  GetReaderState: cardinal;
    procedure CloseAndDisconnect;
    procedure CardInsertedAction;
    procedure CardActiveAction;
    procedure CardRemovedAction;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Init: boolean;
    function    Open: boolean;
    procedure   Close;
    function    Connect: boolean;
    procedure   Disconnect;
    function    GetResponseFromCard(const apdu: string): string; overload;
    function    GetResponseFromCard(const command: string; var data: string; var sw1, sw2: byte): boolean; overload;

    function    GSMStatus: integer;
    function    GSMSelect(const FileID: string): integer;
    function    GSMReadBinary(const Offset, Length: integer; var Data: string): integer;

  published
    property UseReaderNum: integer    read FUseReaderNum    write SetReaderNum  default -1;

    property OnCardInserted:     TNotifyEvent    read FOnCardInserted     write FOnCardInserted;
    property OnCardActive:       TNotifyEvent    read FOnCardActive       write FOnCardActive;
    property OnCardRemoved:      TNotifyEvent    read FOnCardRemoved      write FOnCardRemoved;
    property OnCardInvalid:      TNotifyEvent    read FOnCardInvalid      write FOnCardInvalid;
    property OnReaderWaiting:    TNotifyEvent    read FOnReaderWaiting    write FOnReaderWaiting;
    property OnReaderListChange: TNotifyEvent    read FOnReaderListChange write FOnReaderListChange;
    property OnError:            TPCSCErrorEvent read FOnError            write FOnError;
    property OnCHVNeeded:        TPCSCPinEvent   read FOnCHVNeeded        write FOnCHVNeeded;

    property ReaderList:       TStringList read FReaderList;
    property NumReaders:       integer     read FNumReaders;
    property Connected:        boolean     read FConnected;
    property Opened:           boolean     read IsReaderOpen;
    property ReaderState:      cardinal    read GetReaderState;
    property AttrProtocol:     integer     read FAttrProtocol;
    property AttrICCType:      string      read FAttrICCType;
    property AttrCardATR:      string      read FAttrCardATR;
    property AttrVendorName:   string      read FAttrVendorName;
    property AttrVendorSerial: string      read FAttrVendorSerial;
    property GSMCurrentFile:   string      read FGSMCurrentFile;
    property GSMFileInfo:      string      read FGSMFileInfo;
    property GSMDirInfo:       string      read FGSMDirInfo;
    property GSMVoltage30:     boolean     read FGSMVoltage30;
    property GSMVoltage18:     boolean     read FGSMVoltage18;
  end;

procedure Register;

implementation

var
  ActReaderState  : cardinal;
  LastReaderState : cardinal;
  SelectedReader  : PChar;
  ReaderOpen      : boolean;
  NotifyHandle    : HWND;

const

  // GSM Commands
  GCGetStatus   = #$A0#$F2#$00#$00#$16;
  GCGetResponse = #$A0#$C0#$00#$00;
  GCSelectFile  = #$A0#$A4#$00#$00#$02;
  GCReadBinary  = #$A0#$B0;

  GSMMasterFile  = #$3f#$00;
  DFgsm900       = #$7f#$20;
  DFgsm1800      = #$7f#$21;

procedure Register;
begin
  RegisterComponents('More...', [TPCSCConnector]);
end;

function SortOutSubstrings(const From:string; var t:array of string; const Delim:TDelimiters = [' ',';']; const ConcatDelim:boolean = true):integer;
var a,b,s,i : integer;
    sep     : boolean;
begin
a := 1;
b := Low(t);
s := 1;
i := 0;
sep := ConcatDelim;
t[b] := '';

while a <= Length(From) do
  begin
  if not (From[a] in Delim) then
     begin
     Inc(i);
     sep := false;
     end else
     begin
     if not sep then
        begin
        t[b] := Copy(From, s, i);
        Inc(b);
        if b > High(t) then Break;
        t[b] := '';
        end;
     if ConcatDelim then sep := true;
     s := a + 1;
     i := 0;
     end;
  Inc(a);
  end;
if (b <= High(t)) and (i > 0) then
   begin
   t[b] := Copy(From, s, i);
   Inc(b);
   end;
for a := b + 1 to High(t) do t[a] := '';
Result := b;
end;

function OrdD(const From: string; const Index: integer): integer;
begin
if Index <= Length(From) then Result := Ord(From[Index])
                         else Result := 0;
end;

function CardWatcherThread(PContext: pointer): integer;
var
  RetVar   : cardinal;
  RContext : cardinal;
  RStates  : array[0..1] of SCARD_READERSTATEA;
begin
  try
  RContext := cardinal(PContext^);
  FillChar(RStates,SizeOf(RStates),#0);
  RStates[0].szReader     := SelectedReader;
  RStates[0].pvUserData   := nil;
  RStates[0].dwEventState := ActReaderState;
  while ReaderOpen do
    begin
    RStates[0].dwCurrentState := RStates[0].dwEventState;
    RetVar := SCardGetStatusChangeA(RContext, -1, RStates, 1);
    ActReaderState := RStates[0].dwEventState;
    PostMessage(NotifyHandle, WM_CARDSTATE, RetVar, 0);
    end;
  finally
    Result := 0;
  end;
end;

procedure TPCSCConnector.MessageWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_CARDSTATE) then
    begin
    if Msg.WParam <> SCARD_S_SUCCESS then
      if Assigned(FOnError) then FOnError(Self, esGetStatus, Msg.WParam);
    if ActReaderState <> LastReaderState then
      begin
      ProcessReaderState(LastReaderState, ActReaderState);
      end;
    end
    else Msg.Result := DefWindowProc(NotifyHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

constructor TPCSCConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReaderList   := TStringlist.Create;
  FContext      := 0;
  FCardHandle   := 0;
  FNumReaders   := 0;
  FUseReaderNum := -1;
  FConnected    := false;
  ActReaderState  := SCARD_STATE_UNAWARE;
  LastReaderState := SCARD_STATE_UNAWARE;
  ReaderOpen      := false;
  ClearReaderAttributes;
  ClearCardAttributes;
  if not (csDesigning in ComponentState) then NotifyHandle := AllocateHWnd(MessageWndProc);
end;

destructor TPCSCConnector.Destroy;
begin
  CloseAndDisconnect;
  SCardReleaseContext(FContext);
  FReaderList.Free;
  if not (csDesigning in ComponentState) then DeallocateHWnd(NotifyHandle);
  inherited Destroy;
end;

function TPCSCConnector.Init: boolean;
var
  RetVar         : cardinal;
  ReaderList     : string;
  ReaderListSize : integer;
  v              : array[0..MAXIMUM_SMARTCARD_READERS] of string;
  i              : integer;

begin
  Result      := false;
  FNumReaders := 0;
  CloseAndDisconnect;
  if SCardIsValidContext(FContext) = SCARD_S_SUCCESS then SCardReleaseContext(FContext);
  RetVar := SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, @FContext);
  if RetVar = SCARD_S_SUCCESS then
    begin
    ReaderListSize := 0;
    RetVar := SCardListReadersA(FContext, nil, nil, ReaderListSize);
    if RetVar = SCARD_S_SUCCESS then
      begin
      SetLength(ReaderList, ReaderListSize);
      SCardListReadersA(FContext, nil, Pointer(ReaderList), ReaderListSize);
      FReaderList.Clear;
      SortOutSubstrings(ReaderList,v,[#0]);
      for i := 0 to MAXIMUM_SMARTCARD_READERS do
        if v[i] <> '' then FReaderList.Add(v[i]);
      FNumReaders := FReaderList.Count;
      if FNumReaders > 0 then
        begin
        if Assigned(FOnReaderListChange) then FOnReaderListChange(Self);
        Result := true;
        end;
      end else if Assigned(FOnError) then FOnError(Self, esInit, RetVar);
    end else if Assigned(FOnError) then FOnError(Self, esInit, RetVar);
end;

function TPCSCConnector.Open: boolean;
var
  ThreadID    : LongWord;
begin
  CloseAndDisconnect;
  if (FUseReaderNum > NOREADERSELECTED) and
     (SCardIsValidContext(FContext) = SCARD_S_SUCCESS) then
    begin
    ReaderOpen      := true;
    ActReaderState  := SCARD_STATE_UNAWARE;
    LastReaderState := SCARD_STATE_UNAWARE;
    BeginThread(nil, 0, CardWatcherThread, @FContext, 0, ThreadID);
    Result := true;
    end else Result := false;
end;

procedure TPCSCConnector.Close;
begin
  ReaderOpen := false;
  SCardCancel(FContext);
  if FConnected then Disconnect;
end;

function TPCSCConnector.Connect: boolean;
begin
  if FConnected then Disconnect;
  if FUseReaderNum > NOREADERSELECTED then
    if ConnectSelectedReader then FConnected := true
                             else FConnected := false;
  Result := FConnected;
end;

procedure TPCSCConnector.Disconnect;
begin
  if FConnected then
    begin
    SCardDisconnect(FCardHandle, SCARD_RESET_CARD);
    FConnected  := false;
    FCardHandle := 0;
    end;
end;

procedure TPCSCConnector.CloseAndDisconnect;
begin
  if FConnected then Disconnect;
  if ReaderOpen then Close;
end;

function TPCSCConnector.ConnectSelectedReader: boolean;
var
  RetVar : cardinal;
begin
  RetVar := SCardConnectA(FContext,
                          SelectedReader,
                          SCARD_SHARE_EXCLUSIVE,
                          SCARD_PROTOCOL_Tx,
                          FCardHandle,
                          @FAttrProtocol);
  case RetVar of
    SCARD_S_SUCCESS      : begin
                           CardActiveAction;
                           Result := true;
                           end;
    SCARD_W_REMOVED_CARD : begin
                           Result := true;
                           end;
    else                   begin
                           Result := false;
                           if Assigned(FOnError) then FOnError(Self, esConnect, RetVar);
                           end;
    end;
end;

procedure TPCSCConnector.ProcessReaderState(const OldState,NewState: cardinal);
var
  CardInOld, CardInNew     : boolean;
  ReaderEmOld, ReaderEmNew : boolean;
  CardMuteOld, CardMuteNew : boolean;
  CardIgnore               : boolean;

begin
CardInOld   := (OldState and SCARD_STATE_PRESENT) > 0;
CardInNew   := (NewState and SCARD_STATE_PRESENT) > 0;
ReaderEmOld := (OldState and SCARD_STATE_EMPTY) > 0;
ReaderEmNew := (NewState and SCARD_STATE_EMPTY) > 0;
CardMuteOld := (OldState and SCARD_STATE_MUTE) > 0;
CardMuteNew := (NewState and SCARD_STATE_MUTE) > 0;
CardIgnore  := (NewState and SCARD_STATE_IGNORE) > 0;

if CardMuteNew     and
   not CardMuteold then if Assigned(FOnCardInvalid) then FOnCardInvalid(Self);

if CardInNew       and
   not CardInOld   and
   not CardMuteNew and
   not CardIgnore  then CardInsertedAction;

if CardInOld     and
   not CardInNew then CardRemovedAction;

if ReaderEmNew     and
   not ReaderEmOld then begin
                        if Assigned(FOnReaderWaiting) then FOnReaderWaiting(Self);
                        end;

LastReaderState := NewState;
end;

procedure TPCSCConnector.CardInsertedAction;
begin
  if Assigned(FOnCardInserted) then FOnCardInserted(Self);
  if FConnected then CardActiveAction;
end;

procedure TPCSCConnector.CardActiveAction;
begin
  GetReaderAttributes;
  if FAttrProtocol <> SCARD_PROTOCOL_UNK then
    begin
    GetCardAttributes;
    if Assigned(FOnCardActive) then FOnCardActive(Self);
    end;
end;

procedure TPCSCConnector.CardRemovedAction;
begin
  ClearReaderAttributes;
  ClearCardAttributes;
  if Assigned(FOnCardRemoved) then FOnCardRemoved(Self);
  Disconnect;
end;

procedure TPCSCConnector.SetReaderNum(Value: Integer);
begin
  if Value <> FUseReaderNum then
    begin
    CloseAndDisconnect;
    if Value < FReaderList.Count then
      begin
      SelectedReader := PChar(FReaderList[Value]);
      FUseReaderNum   := Value;
      end else
      begin
      SelectedReader := '';
      FUseReaderNum   := -1;
      end;
    end;
end;

function TPCSCConnector.IsReaderOpen: boolean;
begin
  Result := ReaderOpen;
end;

function TPCSCConnector.GetReaderState: cardinal;
begin
  Result := ActReaderState;
end;

procedure TPCSCConnector.GetReaderAttributes;
var
  RetVar : cardinal;
  ABuf   : string;
  AIBuf  : integer;
  ALen   : integer;
begin
  ABuf := StringOfChar(#0, 127);
  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ATR_STRING, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrCardATR := Copy(ABuf, 1, ALen)
                              else FAttrCardATR := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_NAME, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrVendorName := Copy(ABuf, 1, ALen)
                              else FAttrVendorName := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_IFD_SERIAL_NO, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrVendorSerial := Copy(ABuf, 1, ALen)
                              else FAttrVendorSerial := '';

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_CURRENT_PROTOCOL_TYPE, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrProtocol := AIBuf
                              else FAttrProtocol := 0;

  ALen := SizeOf(AIBuf);
  AIBuf := 0;
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ICC_TYPE_PER_ATR, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then begin
                                   case AIBuf of
                                     1  : FAttrICCType := 'ISO7816A';
                                     2  : FAttrICCType := 'ISO7816S';
                                     else FAttrICCType := 'UNKNOWN';
                                     end;
                                   end
                              else FAttrICCType := '';
end;

procedure TPCSCConnector.GetCardAttributes;
begin
if GSMSelect(DFgsm900) = GSMStatusOK then
  begin
  FGSMVoltage30 := (OrdD(FGSMDirInfo, 14) and $10) > 0;
  FGSMVoltage18 := (OrdD(FGSMDirInfo, 14) and $20) > 0;
  end;
end;

procedure TPCSCConnector.ClearReaderAttributes;
begin
  FAttrCardATR      := '';
  FAttrVendorName   := '';
  FAttrVendorSerial := '';
  FAttrProtocol     := 0;
  FAttrICCType      := '';
end;

procedure TPCSCConnector.ClearCardAttributes;
begin
  FGSMCurrentFile := '';
  FGSMFileInfo    := '';
  FGSMDirInfo     := '';
  FGSMVoltage30   := false;
  FGSMVoltage18   := false;
end;

function TPCSCConnector.GetResponseFromCard(const APdu: string): string;
var
  RetVar : cardinal;
  SBuf   : string;
  SLen   : cardinal;
  RBuf   : string;
  RLen   : cardinal;
  Ppci   : Pointer;
begin
SBuf := APdu;
RBuf := StringOfChar(#0,MAXAPDULENGTH);
if Length(SBuf) <= MAXAPDULENGTH then
  begin
  case FAttrProtocol of
    SCARD_PROTOCOL_T0 : Ppci := @SCARD_PCI_T0;
    SCARD_PROTOCOL_T1 : Ppci := @SCARD_PCI_T1;
    else                Ppci := nil;
    end;
  SLen := Length(APdu);
  RLen := Length(RBuf);
  RetVar := SCardTransmit(FCardHandle, Ppci, Pointer(SBuf), SLen, nil, Pointer(RBuf), @RLen);
  if RetVar = SCARD_S_SUCCESS then
    begin
    Result := Copy(RBuf,1,RLen);
    end else
    begin
    Result := '';
    if Assigned(FOnError) then FOnError(Self, esTransmit, RetVar);
    end;
  end;
end;

function TPCSCConnector.GetResponseFromCard(const Command: string; var Data: string; var sw1, sw2: byte): boolean;
var
  Answer  : string;
  AnswerL : integer;
begin
Answer := GetResponseFromCard(Command + Data);
AnswerL := Length(Answer);
if AnswerL >= 2 then
  begin
  Data := Copy(Answer, 1, AnswerL - 2);
  sw1  := Ord(Answer[AnswerL - 1]);
  sw2  := Ord(Answer[AnswerL]);
  if sw1 = GSMStatusResponseData then
    begin
    Data := Chr(sw2);
    if not GetResponseFromCard(GCGetResponse, Data, sw1, sw2) then
      begin
      Data := '';
      sw1  := 0;
      sw2  := 0;
      Result := false;
      end else Result := true;
    end else Result := true;
  end else
  begin
  Data := '';
  sw1  := 0;
  sw2  := 0;
  Result := false;
  end;
end;

function TPCSCConnector.GSMStatus: integer;
var
  Answer   : string;
  sw1, sw2 : byte;
begin
  GetResponseFromCard(GCGetStatus, Answer, sw1, sw2);
  Result := (sw1 shl 8) + sw2;
  if Result = GSMStatusOK then
    begin
    FGSMDirInfo := Answer;
    FGSMCurrentFile := Copy(Answer, 5, 2);
    end else
    begin
    FGSMDirInfo := '';
    end;
end;

function TPCSCConnector.GSMSelect(const FileID: string): integer;
var
  Answer   : string;
  sw1, sw2 : byte;
begin
  Answer := FileID;
  GetResponseFromCard(GCSelectFile, Answer, sw1, sw2);
  Result := (sw1 shl 8) + sw2;
  if Result = GSMStatusOK then
    begin
    FGSMCurrentFile := Copy(Answer, 5, 2);
    if OrdD(Answer, 7) = GSMFileTypeEF then
      begin
      FGSMFileInfo := Answer;
      end else
      begin
      FGSMDirInfo := Answer;
      end;
    end;
end;

function TPCSCConnector.GSMReadBinary(const Offset, Length: integer; var Data: string): integer;
var
  Command  : string;
  sw1, sw2 : byte;
begin
  Command := GCReadBinary + Chr(Offset div 256) + Chr(Offset mod 256) + Chr(Length mod 256);
  GetResponseFromCard(Command, Data, sw1, sw2);
  Result := (sw1 shl 8) + sw2;
  if Result = GSMStatusOK then
    begin
    end;
end;

end.

