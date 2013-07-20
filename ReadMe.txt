The PCSC Connector Component for Delphi

The component has the following properties, methods and events:


Methods:

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

      Obvious what these do

    function Init: boolean;

      Init tries to establish a connection with the SmartCard API and to read the
      list of installed readers. If the connection is successfull, and a minimum
      of one reader is found, TRUE is returned

    function Open: boolean;

      Open starts watching the selected reader for status changes

    procedure   Close;

      Close cancels all open requests and finishes watching the reader

    function Connect: boolean;

      Connect tries to connect to the card in the selected reader.
      If successful, TRUE is returned

    procedure Disconnect;

      Disconnect disconnects from the card in the selected reader

    function GetResponseFromCard(const apdu: string): string;
    function GetResponseFromCard(const command : string; 
                                 var   data    : string; 
                                 var   sw1, sw2: byte): boolean;

      Let you send APDUs to the card. Either provide a complete APDU and get the
      resulting APDU as a string, or provide the command (CLA,INS,P1,P2,P3) and DATA and get
      SW1, SW2 and DATA as response and TRUE as result if the request was successful


Properties:

    property ReaderList:   TStringList;
    property UseReaderNum: integer;

      ReaderList holds the list of available readers. It is filled by calling Init.
      UseReaderNum points to the Reader to use in ReaderList or is set to -1
      if no reader is selected

    property NumReaders:       integer;

      Holds the number of available readers

    property Connected:        boolean;

      TRUE if connected to the reader (the reader MAY BE empty)

    property Opened:           boolean;

      TRUE if a reader is being watched

    property ReaderState:      cardinal;

      holds the last status of the reader, see WinSCard.pas

    property AttrProtocol:     integer;

      the protocol used to communicate with the current card

    property AttrICCType:      string;

      the ICC type of the current card

    property AttrCardATR:      string;

      the ATR of the current card

    property AttrVendorName:   string;

      the vendor name of the reader

    property AttrVendorSerial: string;

      the serial number of the reader


Events:

    property OnCardInserted :     TPCSCCardEvent

      fires when a card is inserted in the reader

    property OnCardActive :       TPCSCCardEvent

      fires when a inserted card is recognised and may be accessed.
      The Attr-propertys are filled beforehand

    property OnCardRemoved :      TPCSCCardEvent

      fires when a inserted card is removed from the reader

    property OnCardInvalid :      TPCSCCardEvent

      fires when a inserted card is not readable

    property OnReaderWaiting :    TPCSCReaderEvent

      fires when the reader is ready and waiting for a card

    property OnReaderListChange : TPCSCReaderEvent

      fires when the reader list has been updated

    property OnError :            TPCSCErrorEvent

    TPCSCErrorEvent = procedure(Sender:    TObject; 
                                ErrSource: TErrSource;
                                ErrCode:   cardinal)

    TErrSource      = (esInit, esGetStatus, esConnect, esTransmit);

      fires when a error message is received while connecting to the card,
      reading the status word or transmitting data to the card


Hints:

  --------------------------------------------------------

  First, call INIT

  Then, set the reader you want to use

  Then, call OPEN

    or

  call OPEN in the On ReaderWaiting Event

  Then, if a card is inserted, you may call GetResponse...

  --------------------------------------------------------

  A error 0x80100002 (The action was cancelled by an SCardCancel request)
  is normal behaviour when calling CLOSE

  --------------------------------------------------------
 
