{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrAbecsPinPad;

interface

uses
  SysUtils, Classes, Contnrs,
  ACBrBase, ACBrDevice;

resourcestring
  CERR_INVBLK = 'Invalid Block';
  CERR_INVBLKSIZE = 'Block is Larger then %d bytes';
  CERR_INVTLVSIZE = 'TLV is Larger then %d bytes';
  CERR_INVCMD = 'Invalid Command';
  CERR_INVRSP = 'Invalid Response';
  CERR_INVRSPSTAT = 'Invalid Response Status';
  CERR_INVTLV = 'Invalid TLV data';
  CERR_NOSTART = 'Packet do not START with SYN';
  CERR_NOSTOP = 'Packet do not have ETB';
  CERR_INVCRC = 'Packet has invalid CRC';

  CERR_NOTENABLED = 'Communication is not Enabled';
  CERR_BUSY = 'Communication is Busy';

  CERR_READING_ACK = 'Error reading ACK';
  CERR_READING_RSP = 'Error reading Response';
  CERR_TIMEOUT_RSP = 'Timeout reading Response';
  CERR_READING_CAN = 'Error waiting for CAN response';
  CERR_CANCELLED_BY_USER = 'Cancelled by User';
  CERR_DATAPACKET_TO_LARGE = 'Data Packet is Larger than %d';

const
  // control characters
  ACK = 06; // Sent from the pinpad to the SPE when receiving a valid packet.
  NAK = 21; // It is returned to the side that sent an invalid packet, requesting its retransmission.

  EOT = 04;  // Pinpad response when receiving a «CAN».
  CAN = 24;  // Sent from the SPE to the pinpad to cancel the execution of a command.

  DC3 = 19; // Substitution byte, to prevent special bytes from traveling in the body of the packet.
  SYN = 22; // Indicates the start of a packet.
  ETB = 23; // Indicates the end of a packet.

  MAX_BLOCK_SIZE = 999;
  MAX_TLV_SIZE = MAX_BLOCK_SIZE - 4;
  MAX_PACKET_SIZE = 2049;

  TIMEOUT_ACK = 2000;
  TIMEOUT_RSP = 10000;
  MAX_ACK_TRIES = 3;

  // Return Codes
  ST_OK            = 000; // Command executed successfully.
  ST_NOSEC         = 003; // Attempted to use “Secure Communication” when it has not been established.
  ST_F1            = 004; // Function #1 key pressed.
  ST_F2            = 005; // Function #2 key pressed.
  ST_F3            = 006; // Function #3 key pressed.
  ST_F4            = 007; // Function #4 key pressed.
  ST_BACKSP        = 008; // Clear (backspace) key pressed
  ST_ERRPKTSEC     = 009; // Error decoding data received via “Secure Communication”; or Cleartext command received with “Secure Communication” established.
  ST_INVCALL       = 010; // Invalid call to a command (previous operations are necessary) or unknown command (in case of an “ERR” response).
  ST_INVPARM       = 011; // An invalid parameter was passed to the command.
  ST_TIMEOUT       = 012; // The maximum time stipulated for the operation has been exhausted.
  ST_CANCEL        = 013; // Operation canceled by the cardholder.
  ST_MANDAT        = 019; // A mandatory parameter was not sent by the SPE.
  ST_TABVERDIF     = 020; // EMV Tables version differs from the expected.
  ST_TABERR        = 021; // Error when trying to write tables (lack of space, for example).
  ST_INTERR        = 040; // Internal pinpad error (unexpected situation that does not correspond to the other error codes described here).
  ST_MCDATAERR     = 041; // Magnetic card reading error.
  ST_ERRKEY        = 042; // MK / DUKPT referenced is not present in the pinpad.
  ST_NOCARD        = 043; // There is no ICC present in the coupler or CTLS detected by the antenna.
  ST_PINBUSY       = 044; // Pinpad cannot process PIN capture temporarily due to security constrains (such as when the capture limit is reached within a time interval).
  ST_RSPOVRFL      = 045; // Response data exceeds the maximum allowed size.
  ST_ERRCRYPT      = 046; // Generic cryptographic validation error.
  ST_DUMBCARD      = 060; // ICC inserted, but not responding (“mute”).
  ST_ERRCARD       = 061; // Communication error between the pinpad and the ICC or CTLS.
  ST_CARDINVALIDAT = 067; // ICC is invalidated.
  ST_CARDPROBLEMS  = 068; // ICC with problems. This status is valid for many situations in which the ICC does not behave as expected and the transaction must be terminated.
  ST_CARDINVDATA   = 069; // The ICC behaves correctly but has invalid or inconsistent data.
  ST_CARDAPPNAV    = 070; // ICC with no matching application.
  ST_CARDAPPNAUT   = 071; // The application selected in the ICC cannot be used in this situation.
  ST_ERRFALLBACK   = 076; // High level error in the ICC that allows fallback to magnetic stripe.
  ST_INVAMOUNT     = 077; // Invalid amount for the transaction.
  ST_ERRMAXAID     = 078; // Number of candidate AIDs exceeds the processing capacity of the EMV kernel.
  ST_CARDBLOCKED   = 079; // Card is blocked.
  ST_CTLSMULTIPLE  = 080; // More than one CTLS was presented to the reader simultaneously.
  ST_CTLSCOMMERR   = 081; // Communication error between the pinpad (antenna) and the CTLS.
  ST_CTLSINVALIDAT = 082; // CTLS is invalidated.
  ST_CTLSPROBLEMS  = 083; // CTLS with problems. This status is valid for many situations in which the CTLS does not behave as expected and the transaction must be terminated.
  ST_CTLSAPPNAV    = 084; // CTLS with no matching application.
  ST_CTLSAPPNAUT   = 085; // The application selected in the CTLS cannot be used in this situation.
  ST_CTLSEXTCVM    = 086; // Cardholder must perform a validation on his device (mobile phone, for example) and then re-present it to the pinpad.
  ST_CTLSIFCHG     = 087; // CTLS processing resulted in “change interface” (request ICC or magnetic card).
  ST_MFNFOUND      = 100; // Media file not found.
  ST_MFERRFMT      = 101; // Media file format error.
  ST_MFERR         = 102; // Media file loading error.

  // List of parameters
  SPE_IDLIST   = $0001; // B..128 (n*X2,n<=64) List of return data identifiers (up to 64).
  SPE_MTHDPIN  = $0002; // N1 Method to be used for PIN encryption:
                        // “1” = MK/WK:TDES:PIN; and
                        // “3” = DUKPT:TDES:PIN (see section 5.1.1).
  SPE_MTHDDAT  = $0003; // N2 Method to be used for data encryption:
                        // “10” = MK/WK:TDES:DAT (ECB block encryption);
                        // “11” = MK/WK:TDES:DAT (CBC block encryption);
                        // “50” = DUKPT:TDES:DAT#3 (ECB block encryption, see section 5.1.1); and
                        // “51” = DUKPT:TDES:DAT#3 (CBC block encryption, see section 5.1.1).
  SPE_TAGLIST  = $0004; // B..128 List of tags referring to the EMV objects required by the SPE.
  SPE_EMVDATA  = $0005; // B..512 EMV objects sent to the pinpad (in TLV format - see section 7.1).
  SPE_CEXOPT   = $0006; // A6 “CEX” command options.
                        // “0xxxxx” = Ignore keys;
                        // “1xxxxx” = Verify key pressing.
                        // “x0xxxx” = Ignore magnetic card;
                        // “x1xxxx” = Verify magnetic card swiping.
                        // “xx0xxx” = Ignore ICC;
                        // “xx1xxx” = Verify ICC insertion;
                        // “xx2xxx” = Verify ICC removal.
                        // “xxx0xx” = Ignore CTLS (do not activate antenna);
                        // “xxx1xx” = Activate antenna and verify CTLS presence.
                        // “xxxx00” = RFU.
  SPE_TRACKS   = $0007; // N4 Identification of track data to be returned by the pinpad in “GTK” command.
  SPE_OPNDIG   = $0008; // N1 Number of numeric digits (even number) to be preserved as cleartext at the beginning of encrypted tracks (accepted values: “0”, “2”, “4”, “6”, “8”).
  SPE_KEYIDX   = $0009; // N2 DUPKT or MK slot index (“00” to “99”)
  SPE_WKENC    = $000A; // B16 Working Key encrypted by MK:TDES.
  SPE_MSGIDX   = $000B; // X2 Index to the message to be presented.
  SPE_TIMEOUT  = $000C; // X1 Wait time for a cardholder action (in seconds - up to 255). IMPORTANT: This parameter reflects the cardholder inactivity time and not the maximum command execution time.
  SPE_MINDIG   = $000D; // X1 Minimum number of digits to be captured on the pinpad (from 0 to 32).
  SPE_MAXDIG   = $000E; // X1 Maximum number of digits to be captured on the pinpad (from 0 to 32).
  SPE_DATAIN   = $000F; // B..995 Generic data to be sent to the pinpad.
  SPE_ACQREF   = $0010; // N2 Acquirer identifier for searching the AID Tables (de “01” a “99”).
  SPE_APPTYPE  = $0011; // N..20 Application type identifiers for searching the AID Tables (from "01" to "98"). This field supports 1 to 10 different identifiers.
  SPE_AIDLIST  = $0012; // A..512 Specific list of records in the AID Tables to be used in the transaction processing, which can include up to 128 entries in the “AARR” format, as follows:
                        // "AA" = Identifier of the Acquirer responsible for the table (from "01" to "99"); and
                        // "RR" = Index of the record in the table (from "01" to "ZZ").
  SPE_AMOUNT   = $0013; // N12 Transaction amount (Amount, authorized), in cents.
  SPE_CASHBACK = $0014; // N12 Cashback amount (Amount, other) , in cents.
  SPE_TRNDATE  = $0015; // N6 Transaction date (“AAMMDD”)
  SPE_TRNTIME  = $0016; // N6 Transaction time (“HHMMSS”)
  SPE_GCXOPT   = $0017; // N5 “GCX” command options:
                        // “0xxxx” = Wait for magnetic card or ICC; or
                        // “1xxxx” = Wait for magnetic card; ICC or CTLS;
                        // “x0xxx” = Show transaction amount on the card waiting prompt, if not zero.
                        // “x1xxx” = Do not show transaction amount.
                        // “xx000” = RFU.
  SPE_GOXOPT   = $0018; // N5 “GOX” command options:
                        // “1xxxx” = PAN is in the Exception List (only for ICC EMV).
                        // “x1xxx” = Transaction shall not be offline approved (only for ICC EMV).
                        // “xx1xx” = Do not allow PIN bypass.
                        // “xxx00” = RFU.
  SPE_FCXOPT   = $0019; // N4 “FCX” command options:
                        // “0xxx” = Transaction approved by the Acquirer.
                        // “1xxx” = Transaction declined by the Acquirer.
                        // “2xxx” = Unable to go online (or invalid response from the Acquirer).
                        // “x000” = RFU.
  SPE_TRMPAR   = $001A; // B10 Terminal Risk Management parameters to be used on “GOX”:
                        // - Terminal Floor Limit (“X4” format, in cents);
                        // - Target Percentage to be used for Biased Random Selection (“X1” format);
                        // - Threshold Value for Biased Random Selection (“X4” format, in cents); and
                        // - Maximum Target Percentage to be used for Biased Random Selection (“X1” format).
  SPE_DSPMSG   = $001B; // S..128 Display message in free format, may have line break characters (0Dh). When formatting this message, the SPE shall respect the pinpad display capabilities (see PP_DSPTXTSZ).
  SPE_ARC      = $001C; // A2 Authorization Response Code (approval/declination code returned by the Acquirer).
  SPE_IVCBC    = $001D; // B8 “IV” (Initialization Vector) to be used in CBC block cryptography
  SPE_MFNAME   = $001E; // A8 Media file name (only numeric characters and letters, without spaces or symbols).
                        // The file name is not case sensitive, that is, the names “ImgAlt01” and “IMGALT01” represent the same file.
  SPE_MFINFO   = $001F; // B10 Information about the media file:
                        // X4 = Size (de 0 a 4294967295 bytes).
                        // B2 = CRC of the file.
                        // B1 =Type (01h = PNG, 02h = JPG, 03h = GIF, other values = RFU); and
                        // B3 = RFU (000000h).
  SPE_MNUOPT   = $0020; // S..24 Text with a menu option.
  SPE_TRNTYPE  = $0021; // B1 Transaction type to be performed:
                        // 00h = Payment;
                        // 01h = Cash;
                        // 09h = Payment with cashback;
                        // 20h = Refund;
                        // 30h = Balance inquiry; or Other values according to ISO 8583:1987.
  SPE_TRNCURR  = $0022; // N3 Currency code to be used in the transaction (ex.: Real = “986”, Dollar = “840”).
  SPE_PANMASK  = $0023; // N4 PAN masking definition in “LLRR” format:
                        // “LL” = Number of open digits on the left; and
                        // “RR” = Number of open digits on the right.
  SPE_PBKMOD   = $0024; // B256 RSA public key modulus (2048 bits).
  SPE_PBKEXP   = $0025; // B..3 RSA public key exponent.


  // List of return data fields
  PP_SERNUM    = $8001; // A..32 Pinpad serial number (free format, it depends on the manufacturer).
  PP_PARTNBR   = $8002; // A..32 Pinpad part number (free format, it depends on the manufacturer).
  PP_MODEL     = $8003; // A..20 Model / hardware version, in the format: “xx...xx;m...m”, where: “xx...xx” is the device name; and “m...m” is the memory capacity (“512KB”, “1MB”, “2MB”, ...).
  PP_MNNAME    = $8004; // A..20 Name of the manufacturer (free format).
  PP_CAPAB     = $8005; // A10 Pinpad capabilities: “0xxxxxxxxx” = Does not support CTLS; “1xxxxxxxxx” = Supports CTLS. “x0xxxxxxxx” = Display is not graphic; “x1xxxxxxxx” = Monochromatic graphic display; “x2xxxxxxxx” = Color graphic display. “xx00000000” = RFU. PP_SOVER (†) 8006h A..20 Basic software or operating system version (free format).
  PP_SPECVER   = $8007; // A4 Specification version, in “V.VV” format (in this case, fixed “2.12”)
  PP_MANVERS   = $8008; // A16 “Manager” application version, in the format “VVV.VV YYMMDD”.
  PP_APPVERS   = $8009; // A16 “Abecs” application version, in the format “VVV.VV YYMMDD”.
  PP_GENVERS   = $800A; // A16 “Extension” application version, in the format “VVV.VV YYMMDD”.
  PP_KRNLVER   = $8010; // A..20 ICC EMV kernel version.
  PP_CTLSVER   = $8011; // A..20 CTLS EMV kernel version (general or entry point).
  PP_MCTLSVER  = $8012; // A..20 CTLS EMV kernel version - MasterCard PayPass.
  PP_VCTLSVER  = $8013; // A..20 CTLS EMV kernel version - VISA PayWave.
  PP_AECTLSVER = $8014; // A..20 CTLS EMV kernel version - American Express.
  PP_DPCTLSVER = $8015; // A..20 CTLS EMV kernel version - Discover.
  PP_PUREVER   = $8016; // A..20 CTLS EMV kernel version - Pure.
  PP_DSPTXTSZ  = $8020; // N4 Maximum number of rows and columns of the display for showing messages in text mode (“RRCC” format).
  PP_DSPGRSZ   = $8021; // N8 Maximum number of rows and columns of the graphic display for image presentation (“RRRRCCCC” format, in pixels).
  PP_MFSUP     = $8022; // A..20 Supported media file types: “1xxx ...” = Supports PNG format; “x1xx ...” = Supports JPG format. “xx1x ...” = Supports GIF format. --------- 8030h --- Reserved.--------- 8031h --- Reserved.
  PP_MKTDESP   = $8032; // A100 100 characters representing the MK:TDES:PIN key map contained in the pinpad, with each character corresponding to a position (from “00” to “99”), indicating:
                        // “0” = Key absent (not loaded);
                        // “1” = Key present (loaded); and
                        // “2” = Slot not supported.
  PP_MKTDESD   = $8033; // A100 Same for MK:TDES:DAT slots. --------- 8034h --- Reserved.
  PP_DKPTTDESP = $8035; // A100 Same for DUKPT:TDES:PIN slots.
  PP_DKPTTDESD = $8036; // A100 Same for DUKPT:TDES:DAT slots.
  PP_EVENT     = $8040; // A2 Event detected by the pinpad in the “CEX” command:
                        // “00” = [OK/ENTER] key pressed;
                        // “02” = [] key pressed;
                        // “03” = [] key pressed;
                        // “04” = [F1] key pressed;
                        // “05” = [F2] key pressed;
                        // “06” = [F3] key pressed;
                        // “07” = [F4] key pressed;
                        // “08” = [CLEAR] key pressed;
                        // “13” = [CANCEL] key pressed;
                        // “90” = A magnetic card was swiped;
                        // “91” = ICC removed (or already absent);
                        // “92” = ICC inserted (or already present);
                        // “93” = CTLS not detected in 2 (two) minutes; and
                        // “94” = CTLS detected.
  PP_TRK1INC   = $8041; // A..60 Card Track 1, incomplete (see section 5.4.1)
  PP_TRK2INC   = $8042; // A..30 Card Track 2, incomplete (see section 5.4.1)
  PP_TRK3INC   = $8043; // A..30 Card Track 3, incomplete (see section 5.4.1)
  PP_TRACK1    = $8044; // B..88 Card Track 1 (complete), in cleartext or encrypted (see section 5.4.2.1). Note: Although Track 1 is represented in ASCII, this field follows the “B” format in case the data is encrypted.
  PP_TRACK2    = $8045; // B..28 Card Track 1 (complete), in cleartext or encrypted (see section 5.4.2.2). Each Track 2 symbol occupies a nibble, according to the following code:
                        // 0h (0000) → “0” Ah (1010) → “:” Dh (1101) → “=”
                        //    ...          Bh (1011) → “;” Eh (1110) → “>”
                        // 9h (1001) → “9” Ch (1100) → “<” Fh (1110) → “?”
                        // Data are left aligned, with trailing Fh (“?”) if necessary.
  PP_TRACK3    = $8046; // B..60 Card Track 1 (complete), in cleartext or encrypted (same format as PP_TRACK2).
  PP_TRK1KSN   = $8047; // B10 KSN of DUKPT used for Track 1 encryption.
  PP_TRK2KSN   = $8048; // B10 KSN of DUKPT used for Track 2 encryption.
  PP_TRK3KSN   = $8049; // B10 KSN of DUKPT used for Track 3 encryption.
  PP_ENCPAN    = $804A; // B..16 Card PAN, in cleartext of encrypted (see section 5.4.2.2). Each digit of the PAN occupies a nibble. Data is left aligned with trailing Fh, if necessary. Example: A PAN “9781234789432” is encoded as: 97h 81h 23h 47h 89h 43h 2Fh.
  PP_ENCPANKSN = $804B; // B10 KSN of DUKPT used for PAN encryption.
  PP_KSN       = $804C; // B10 KSN of DUKPT used for PIN or data encryption.
  PP_VALUE     = $804D; // A..32 Value captured by the pinpad.
  PP_DATAOUT   = $804E; // B..256 Generic data returned by the pinpad.
  PP_CARDTYPE  = $804F; // N2 “GCX” response: Card type.
                        // “00” = Magnetic;
                        // “03” = ICC EMV;
                        // “05” = CTLS magstripe mode; or
                        // “06” = CTLS EMV.
  PP_ICCSTAT   = $8050; // N1 “GCX” response: Status of the previous ICC processing.
  PP_AIDTABINFO= $8051; // A..120 “GCX” response: Information from the AID Table, which may contain up to 20 concatenated “A6” records.
  PP_PAN       = $8052; // N..19 PAN of the processed card.
  PP_PANSEQNO  = $8053; // N2 Application PAN Sequence Number of the processed card.
  PP_EMVDATA   = $8054; // B..512 List of EMV objects returned by the pinpad (in TLV format - see section 7.1).
  PP_CHNAME    = $8055; // A..26 Cardholder name of the processed card.
  PP_GOXRES    = $8056; // N6 “GOX” response: EMV processing status.
                        // “0xxxxx” = Transaction offline approved;
                        // “1xxxxx” = Transaction declined; or
                        // “2xxxxx” = Transaction requires online approval.
                        // “x1xxxx” = Signature on paper.
                        // “xx1xxx” = Successful offline PIN verification.
                        // “xx2xxx” = PIN captured for online verification.
                        // “xxx1xx” = Cardholder verification performed on the mobile device (smartphone, for example)
                        // “xxxx00” = RFU.
  PP_PINBLK    = $8057; // B8 Encrypted PIN.
  PP_FCXRES    = $8058; // N3 “FCX” response: EMV processing status.
                        // “0xx” = Transaction approved; or
                        // “1xx” = Transaction declined.
                        // “x00” = RFU.
  PP_ISRESULTS = $8059; // B..50 Issuer Script Results (multiple of 5 - up to 10 results).
  PP_BIGRAND   = $805A; // B900 900 random bytes generated by the pinpad (used for testing only).
  PP_LABEL     = $805B; // S..16 Label of the application being processed (ICC EMV or CTLS).
  PP_ISSCNTRY  = $805C; // N3 Issuer Country Code of the processed card.
  PP_CARDEXP   = $805D; // N6 Application Expiration Date of the processed card, in the “YYMMDD” format.
  PP_MFNAME    = $805E; // A8 Name of a media file loaded on the pinpad, always in uppercase.
  PP_DEVTYPE   = $8060; // N2 Device type used in the transaction:
                        // “00” = Card;
                        // “01” = Mobile device (i.e. smartphone);
                        // “02” = Keyring;
                        // “03” = Watch;
                        // “04” = Mobile tag;
                        // “05” = Bracelet;
                        // “06” = Mobile device case/sleeve;
                        // “10” = Tablet or e-reader;
                        // Other values = Future use.
  PP_TLRMEM    = $8062; // X4 Amount of available memory (in bytes) for loading EMV Table records using the “TLR” command.
  PP_ENCKRAND  = $8063; // B256 Random key (KRAND) encrypted by an RSA public key in PKCS # 1 format.
  PP_KSNTDESP00= $9100; // B10 DUKPT:TDES:PIN KSN, slot index #nn (from 00 to 99) IMPORTANT: Pay attention to hexadecimal values (PP_KSNTDESP14 = 910Eh)!!
  PP_KSNTDESP63= $9163;
  PP_KSNTDESD00= $9200; // B10 DUKPT:TDES:DAT KSN, slot index #nn (from 00 to 99) IMPORTANT: Pay attention to hexadecimal values (PP_KSNTDESD79 = 924Fh)!!
  PP_KSNTDESD63= $9263;
  PP_TABVER00  = $9300; // A10 EMV Tables version correspondent to the Acquirer #nn (00 to 99). Index #00 corresponds to the “general” version for all Acquirers.
  PP_TABVER63  = $9363;

type

  EACBrAbecsPinPadError = Exception;

  { TACBrAbecsTLV }

  TACBrAbecsTLV = class
  private
    fData: AnsiString;
    fID: Word;
    function GetAsString: AnsiString;
    function GetSize: Word;
    procedure SetAsString(const AValue: AnsiString);
  public
    constructor Create; overload;
    constructor Create(AID: Word; const AData: AnsiString); overload;
    procedure Clear;
    procedure Assign(Source: TACBrAbecsTLV); reintroduce;

    property ID: Word read fID;
    property Size: Word read GetSize;
    property Data: AnsiString read fData;
    property AsString: AnsiString read GetAsString write SetAsString;
  end;

  { TACBrAbecsTLVList }

  TACBrAbecsTLVList = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrAbecsPar>{$EndIf})
  private
    function GetAsString: AnsiString;
    Function GetItem(Index: Integer): TACBrAbecsTLV;
    procedure SetAsString(const AValue: AnsiString);
    procedure SetItem(Index: Integer; AValue: TACBrAbecsTLV);
  public
    function New: TACBrAbecsTLV;
    Function Add(ATLV: TACBrAbecsTLV): Integer;
    function AddValue(AID: Word; const AData: AnsiString): Integer;
    Procedure Insert(Index: Integer; ATLV: TACBrAbecsTLV);
    property Items[Index: Integer]: TACBrAbecsTLV read GetItem write SetItem; default;
    property AsString: AnsiString read GetAsString write SetAsString;
  end;

  { TACBrAbecsBlock }

  TACBrAbecsBlock = class
  private
    fTLVList: TACBrAbecsTLVList;
    function GetAsString: AnsiString;
    function GetSize: Integer;
    procedure SetAsString(const AValue: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property Size: Integer read GetSize;
    property Values: TACBrAbecsTLVList read fTLVList;

    property AsString: AnsiString read GetAsString write SetAsString;
  end;

  { TACBrAbecsBlockList }

  TACBrAbecsBlockList = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrAbecsBlock>{$EndIf})
  private
    function GetAsString: AnsiString;
    Function GetItem(Index: Integer): TACBrAbecsBlock;
    procedure SetAsString(const AValue: AnsiString);
    procedure SetItem(Index: Integer; AValue: TACBrAbecsBlock);
  public
    function New: TACBrAbecsBlock;
    Function Add(ABlock: TACBrAbecsBlock): Integer;
    Procedure Insert(Index: Integer; ABlock: TACBrAbecsBlock);
    property Items[Index: Integer]: TACBrAbecsBlock read GetItem write SetItem; default;
    property AsString: AnsiString read GetAsString write SetAsString;
  end;

  { TACBrAbecsApplicationLayer }

  TACBrAbecsApplicationLayer = class
  private
    fID: String;
    fBlocks: TACBrAbecsBlockList;
    procedure SetID(AValue: String);

  protected
    function GetAsString: AnsiString; virtual;
    procedure SetAsString(const AValue: AnsiString); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;

    property ID: String read fID write SetID;
    property Blocks: TACBrAbecsBlockList read fBlocks;
    property AsString: AnsiString read GetAsString write SetAsString;

    procedure AddParameter(const PARID: Word; const AData: AnsiString); overload;
    procedure AddParameter(const PARID: Word; AData: TStream); overload;

    function GetParameter(const PARID: Word): AnsiString; overload;
    procedure GetParameter(const PARID: Word; AData: TStream); overload;
  end;

  { TACBrAbecsCommand }

  TACBrAbecsCommand = class( TACBrAbecsApplicationLayer )
  private
    fIsBlocking: Boolean;
  public
    procedure Clear; override;
    property IsBlocking: Boolean read fIsBlocking write fIsBlocking;
  end;

  { TACBrAbecsResponse }

  TACBrAbecsResponse = class( TACBrAbecsApplicationLayer )
  private
    fSTAT: Integer;
  protected
    function GetAsString: AnsiString; override;
    procedure SetAsString(const AValue: AnsiString); override;
  public
    procedure Clear; override;
    property STAT: Integer read fSTAT;
  end;

  { TACBrAbecsPacket }

  TACBrAbecsPacket = class
  private
    fData: AnsiString;

    function GetAsString: AnsiString;
    procedure SetAsString(const AValue: AnsiString);

    function CalcCRC(const AData: AnsiString): AnsiString;
    function DC3Substitution(const AData: AnsiString): AnsiString;
    function DC3RevertSubstitution(const AData: AnsiString): AnsiString;
  public
    constructor Create(const AData: AnsiString = '');
    procedure Clear;

    property Data: AnsiString read fData;
    property AsString: AnsiString read GetAsString write SetAsString;
  end;


  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

  { TACBrAbecsPinPad }

  TACBrAbecsPinPad = class( TACBrComponent )
  private
    fDevice: TACBrDevice;
    fCommand: TACBrAbecsCommand;
    fLogLevel: Byte;
    fResponse: TACBrAbecsResponse;
    fLogFile: String;
    fOnWriteLog: TACBrGravarLog;
    fIsBusy: Boolean;
    fTimeOut: Integer;

    function GetIsEnabled: Boolean;
    function GetPort: String;
    procedure SetIsEnabled(AValue: Boolean);
    procedure SetPort(AValue: String);
  protected
    procedure RegisterLog(const AString: AnsiString; Translate: Boolean = False;
      AddTime: Boolean = True);
    procedure DoException(AException: Exception); overload;
    procedure DoException(const AMsg: String); overload;

    procedure ExecCommand;
    procedure SendCommand;
    function SendCAN: Boolean;
    procedure SendNAK;
    procedure IgnoreAllBytes(SleepTime: Integer = 500);
    function WaitForACK: Byte;
    procedure WaitForResponse;
    procedure EvaluateResponse;
    procedure CancelWaiting;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override ;

    procedure Enable;
    procedure Disable;
    property IsEnabled: Boolean read GetIsEnabled write SetIsEnabled;

    function ReturnCodeDescription(AStatus: Integer): String;
  published
    property Device: TACBrDevice read fDevice;
    property Port: String read GetPort write SetPort;
    property IsBusy: Boolean read fIsBusy;
    property TimeOut: Integer read fTimeOut write fTimeOut default TIMEOUT_RSP;

    property LogFile: String read fLogFile write fLogFile;
    property LogLevel: Byte read fLogLevel write fLogLevel default 1;
    property OnWriteLog: TACBrGravarLog read fOnWriteLog write fOnWriteLog;

    procedure OpenInsecure;
  end ;

implementation

uses
  DateUtils,
  ACBrUtil.FilesIO,
  ACBrUtil.Math,
  synautil;

{ TACBrAbecsTLV }

constructor TACBrAbecsTLV.Create;
begin
  inherited;
  Clear;
end;

constructor TACBrAbecsTLV.Create(AID: Word; const AData: AnsiString);
begin
  if (Length(AData) > MAX_TLV_SIZE) then
    raise EACBrAbecsPinPadError.Create(Format(CERR_INVTLVSIZE, [MAX_TLV_SIZE]));

  Create;
  fID := AID;
  fData := AData;
end;

procedure TACBrAbecsTLV.Clear;
begin
  fID := 0;
  fData := '';
end;

procedure TACBrAbecsTLV.Assign(Source: TACBrAbecsTLV);
begin
  fID := Source.ID;
  fData := Source.Data;
end;

function TACBrAbecsTLV.GetAsString: AnsiString;
begin
  Result := IntToLEStr(fID, 2) +
            IntToLEStr(Size, 2) +
            fData;
end;

function TACBrAbecsTLV.GetSize: Word;
begin
  Result := Length(fData);
end;

procedure TACBrAbecsTLV.SetAsString(const AValue: AnsiString);
var
  l, ld: Integer;
begin
  l := Length(AValue);
  if (l = 0) or (l < 4) then
    raise EACBrAbecsPinPadError.Create(CERR_INVTLV);

  fID := LEStrToInt(copy(AValue, 1, 2));
  ld := LEStrToInt(copy(AValue, 3, 2));
  if (ld > l) then
    raise EACBrAbecsPinPadError.Create(CERR_INVTLV);

  if (ld > MAX_TLV_SIZE) then
    raise EACBrAbecsPinPadError.Create(Format(CERR_INVTLVSIZE, [MAX_TLV_SIZE]));

  fData := copy(AValue, 5, ld);
end;

{ TACBrAbecsTLVList }

function TACBrAbecsTLVList.GetItem(Index: Integer): TACBrAbecsTLV;
begin
  Result := TACBrAbecsTLV(inherited Items[Index]);
end;

procedure TACBrAbecsTLVList.SetItem(Index: Integer; AValue: TACBrAbecsTLV);
begin
  inherited Items[Index] := AValue;
end;

function TACBrAbecsTLVList.New: TACBrAbecsTLV;
begin
  Result := TACBrAbecsTLV.Create;
  Self.Add(Result);
end;

function TACBrAbecsTLVList.Add(ATLV: TACBrAbecsTLV): Integer;
begin
  Result := inherited Add(ATLV);
end;

function TACBrAbecsTLVList.AddValue(AID: Word; const AData: AnsiString): Integer;
var
  tlv: TACBrAbecsTLV;
begin
  tlv := TACBrAbecsTLV.Create(AID, AData);
  Result := Self.Add(tlv);
end;

procedure TACBrAbecsTLVList.Insert(Index: Integer; ATLV: TACBrAbecsTLV);
begin
  inherited Insert(Index, ATLV);
end;

function TACBrAbecsTLVList.GetAsString: AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count-1 do
    Result := Result + Items[i].AsString;
end;

procedure TACBrAbecsTLVList.SetAsString(const AValue: AnsiString);
var
  lt, p, lp: Integer;
  tlv: TACBrAbecsTLV;
begin
  Clear;
  lt := Length(AValue);
  p := 1;
  while (p < lt) do
  begin
    lp := LEStrToInt(copy(AValue, p+2, 2));
    if (lp > lt) then
      raise EACBrAbecsPinPadError.Create(CERR_INVTLV);

    Inc(lp, 4);
    tlv := New;
    tlv.AsString := copy(AValue, p, lp);
    Inc(p, lp);
  end;
end;

{ TACBrAbecsBlock }

constructor TACBrAbecsBlock.Create;
begin
  inherited Create;
  fTLVList := TACBrAbecsTLVList.Create;
  Clear;
end;

destructor TACBrAbecsBlock.Destroy;
begin
  fTLVList.Free;
  inherited Destroy;
end;

procedure TACBrAbecsBlock.Clear;
begin
  fTLVList.Clear;
end;

function TACBrAbecsBlock.GetAsString: AnsiString;
var
  s: AnsiString;
begin
  s := fTLVList.AsString;
  Result := Format('%.3d', [Length(s)]) + s;
end;

function TACBrAbecsBlock.GetSize: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to fTLVList.Count-1 do
    Result := Result + fTLVList[i].Size  + 4; // 4 = CMD_PARID X2 + CMD_PARLEN X2
end;

procedure TACBrAbecsBlock.SetAsString(const AValue: AnsiString);
var
  l: Integer;
  s: AnsiString;
begin
  l := StrToIntDef(copy(AValue,1,3), 0);
  if (l = 0) or (l > (Length(AValue)-3)) then
    raise EACBrAbecsPinPadError.Create(CERR_INVBLK);

  if (l > MAX_BLOCK_SIZE) then
    raise EACBrAbecsPinPadError.Create(Format(CERR_INVBLKSIZE, [MAX_BLOCK_SIZE]));

  s := copy(AValue, 4, l);
  fTLVList.AsString := s;
end;

{ TACBrAbecsBlockList }

function TACBrAbecsBlockList.GetItem(Index: Integer): TACBrAbecsBlock;
begin
  Result := TACBrAbecsBlock(inherited Items[Index]);
end;

procedure TACBrAbecsBlockList.SetItem(Index: Integer; AValue: TACBrAbecsBlock);
begin
  inherited Items[Index] := AValue;
end;

function TACBrAbecsBlockList.New: TACBrAbecsBlock;
begin
  Result := TACBrAbecsBlock.Create;
  Self.Add(Result);
end;

function TACBrAbecsBlockList.Add(ABlock: TACBrAbecsBlock): Integer;
begin
  Result := inherited Add(ABlock);
end;

procedure TACBrAbecsBlockList.Insert(Index: Integer; ABlock: TACBrAbecsBlock);
begin
  inherited Insert(Index, ABlock);
end;

function TACBrAbecsBlockList.GetAsString: AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count-1 do
    Result := Result + Items[i].AsString;
end;

procedure TACBrAbecsBlockList.SetAsString(const AValue: AnsiString);
var
  lt, p, lb: Integer;
  Blk: TACBrAbecsBlock;
begin
  Clear;
  lt := Length(AValue);
  p := 1;
  while (p < lt) do
  begin
    lb := StrToIntDef(copy(AValue, p, 3), 0);
    if (lb = 0) or (lb > lt) then
      raise EACBrAbecsPinPadError.Create(CERR_INVBLK);

    Inc(lb, 3);
    Blk := New;
    Blk.AsString := copy(AValue, p, lb);
    Inc(p, lb);
  end;
end;

{ TACBrAbecsApplicationLayer }

constructor TACBrAbecsApplicationLayer.Create;
begin
  inherited;
  fBlocks := TACBrAbecsBlockList.Create;
  Clear;
end;

destructor TACBrAbecsApplicationLayer.Destroy;
begin
  fBlocks.Free;
  inherited Destroy;
end;

procedure TACBrAbecsApplicationLayer.Clear;
begin
  fID := '';
  fBlocks.Clear;
end;

procedure TACBrAbecsApplicationLayer.AddParameter(const PARID: Word; const AData: AnsiString);
var
  LenData, LenChunk, SpaceLeftBlock, p: Integer;
  LastBlk: TACBrAbecsBlock;
  Chunk: AnsiString;
begin
  LenData := Length(AData);
  if (LenData = 0) then
    Exit;

  if (Blocks.Count = 0) then
    LastBlk := Blocks.New
  else
    LastBlk := Blocks[Blocks.Count-1];

  p := 1;
  while (p < LenData) do
  begin
    Chunk := copy(AData, p, MAX_TLV_SIZE);
    LenChunk := Length(Chunk);

    SpaceLeftBlock := MAX_BLOCK_SIZE - LastBlk.Size;
    if (LenChunk+4 > SpaceLeftBlock) then
      LastBlk := Blocks.New;

    LastBlk.Values.AddValue(PARID, Chunk);
    Inc(p, LenChunk);
  end;
end;

procedure TACBrAbecsApplicationLayer.AddParameter(const PARID: Word; AData: TStream);
var
  LenChunk, SpaceLeftBlock: Integer;
  LastBlk: TACBrAbecsBlock;
  Chunk: AnsiString;
begin
  if not Assigned(AData) then
    Exit;
  if (AData.Size = 0) then
    Exit;

  if (Blocks.Count = 0) then
    LastBlk := Blocks.New
  else
    LastBlk := Blocks[Blocks.Count-1];

  AData.Position := 0;
  while (AData.Position < AData.Size) do
  begin
    Chunk := ReadStrFromStream(AData, MAX_TLV_SIZE);
    LenChunk := Length(Chunk);

    SpaceLeftBlock := MAX_BLOCK_SIZE - LastBlk.Size;
    if (LenChunk+4 > SpaceLeftBlock) then
      LastBlk := Blocks.New;

    LastBlk.Values.AddValue(PARID, Chunk);
  end;
end;

function TACBrAbecsApplicationLayer.GetParameter(const PARID: Word): AnsiString;
var
  i, j: Integer;
  tlv: TACBrAbecsTLV;
begin
  Result := '';
  for i := 0 to Blocks.Count-1 do
  begin
    for j := 0 to Blocks[i].Values.Count-1 do
    begin
      tlv := Blocks[i].Values[j];
      if (tlv.ID = PARID) then
        Result := Result + tlv.Data;
    end;
  end;
end;

procedure TACBrAbecsApplicationLayer.GetParameter(const PARID: Word; AData: TStream);
var
  i, j: Integer;
  tlv: TACBrAbecsTLV;
begin
  AData.Size := 0;
  for i := 0 to Blocks.Count-1 do
  begin
    for j := 0 to Blocks[i].Values.Count-1 do
    begin
      tlv := Blocks[i].Values[j];
      if (tlv.ID = PARID) then
        WriteStrToStream(AData, tlv.Data);
    end;
  end;
end;

procedure TACBrAbecsApplicationLayer.SetID(AValue: String);
begin
  if fID = AValue then
    Exit;

  Clear;
  fID := AValue;
end;

function TACBrAbecsApplicationLayer.GetAsString: AnsiString;
begin
  Result := fID +
            fBlocks.AsString;
end;

procedure TACBrAbecsApplicationLayer.SetAsString(const AValue: AnsiString);
var
  l: Integer;
begin
  Clear;
  l := Length(AValue);
  if (l < 3) then
    raise EACBrAbecsPinPadError.Create(CERR_INVCMD);

  fID := copy(AValue, 1, 3);
  fBlocks.AsString := copy(AValue, 4, l);
end;

{ TACBrAbecsCommand }

procedure TACBrAbecsCommand.Clear;
begin
  fIsBlocking := False;
  inherited Clear;
end;

{ TACBrAbecsResponse }

procedure TACBrAbecsResponse.Clear;
begin
  fSTAT := 0;
  inherited Clear;
end;

function TACBrAbecsResponse.GetAsString: AnsiString;
begin
  Result := inherited GetAsString;
  Insert(Format('%.3d', [fSTAT]), Result, 4);
end;

procedure TACBrAbecsResponse.SetAsString(const AValue: AnsiString);
var
  l, i: Integer;
  s: AnsiString;
begin
  Clear;
  l := Length(AValue);
  if (l < 6) then
    raise EACBrAbecsPinPadError.Create(CERR_INVRSP);

  i := StrToIntDef(copy(AValue, 4, 3), -1);
  if (i < 0) then
    raise EACBrAbecsPinPadError.Create(CERR_INVRSPSTAT);

  fSTAT := i;
  s := AValue;
  Delete(s, 4, 3);
  inherited SetAsString(s);
end;

{ TACBrAbecsPacket }

constructor TACBrAbecsPacket.Create(const AData: AnsiString);
begin
  inherited Create;
  fData := AData;
end;

procedure TACBrAbecsPacket.Clear;
begin
  fData := '';
end;

function TACBrAbecsPacket.GetAsString: AnsiString;
begin
  Result := chr(SYN) +
            DC3Substitution(fData) +
            chr(ETB) +
            CalcCRC(chr(SYN) + fData + chr(ETB));
end;

procedure TACBrAbecsPacket.SetAsString(const AValue: AnsiString);
var
  l: Integer;
  s, crc: AnsiString;
begin
  l := Length(AValue);
  if (copy(AValue, 1, 1) <> chr(SYN)) then
    raise EACBrAbecsPinPadError.Create(CERR_NOSTART);

  if (copy(AValue, l-3, 1) <> chr(ETB)) then
    raise EACBrAbecsPinPadError.Create(CERR_NOSTOP);

  s := copy(AValue, 2, l-4);
  s := DC3RevertSubstitution(s);
  crc := copy(AValue, l-2, 2);
  if (crc <> CalcCRC(s)) then
    raise EACBrAbecsPinPadError.Create(CERR_INVCRC);

  fData := s;
end;

function TACBrAbecsPacket.CalcCRC(const AData: AnsiString): AnsiString;
begin
  Result := IntToLEStr( StringCrcCCITT(AData, 0, $1021), 2);
end;

function TACBrAbecsPacket.DC3Substitution(const AData: AnsiString): AnsiString;
begin
  Result := AData;
  Result := ReplaceString(Result, chr(DC3), chr(DC3)+'3');
  Result := ReplaceString(Result, chr(SYN), chr(DC3)+'6');
  Result := ReplaceString(Result, chr(ETB), chr(DC3)+'7');
end;

function TACBrAbecsPacket.DC3RevertSubstitution(const AData: AnsiString
  ): AnsiString;
begin
  Result := AData;
  Result := ReplaceString(Result, chr(DC3)+'7', chr(ETB));
  Result := ReplaceString(Result, chr(DC3)+'6', chr(SYN));
  Result := ReplaceString(Result, chr(DC3)+'3', chr(DC3));
end;

{ TACBrAbecsPinPad }

constructor TACBrAbecsPinPad.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLogFile := '';
  fLogLevel := 1;
  fTimeOut := TIMEOUT_RSP;
  fOnWriteLog := nil;

  fDevice := TACBrDevice.Create(Self);
  fDevice.Name := 'ACBrDevice';
  {$IFDEF COMPILER6_UP}
  fDevice.SetSubComponent(true);
  {$ENDIF}

  fCommand := TACBrAbecsCommand.Create;
  fResponse := TACBrAbecsResponse.Create;
  fIsBusy := False;
end;

destructor TACBrAbecsPinPad.Destroy;
begin
  Disable;
  fCommand.Free;
  fResponse.Free;
  inherited Destroy;
end;

function TACBrAbecsPinPad.GetIsEnabled: Boolean;
begin
  Result := Self.Device.Ativo;
end;

procedure TACBrAbecsPinPad.SetIsEnabled(AValue: Boolean);
begin
  Self.Device.Ativo := AValue;
end;

procedure TACBrAbecsPinPad.Enable;
begin
  IsEnabled := True;
end;

procedure TACBrAbecsPinPad.Disable;
begin
  CancelWaiting;
  IsEnabled := False;
end;

function TACBrAbecsPinPad.ReturnCodeDescription(AStatus: Integer): String;
begin
  Result := '';
  case AStatus of
    ST_OK            : Result := 'Command executed successfully.';
    ST_NOSEC         : Result := 'Attempted to use “Secure Communication” when it has not been established.';
    ST_F1            : Result := 'Function #1 key pressed.';
    ST_F2            : Result := 'Function #2 key pressed.';
    ST_F3            : Result := ' Function #3 key pressed.';
    ST_F4            : Result := 'Function #4 key pressed.';
    ST_BACKSP        : Result := 'Clear (backspace) key pressed';
    ST_ERRPKTSEC     : Result := 'Error decoding data received via “Secure Communication”; or Cleartext command received with “Secure Communication” established.';
    ST_INVCALL       : Result := 'Invalid call to a command (previous operations are necessary) or unknown command (in case of an “ERR” response).';
    ST_INVPARM       : Result := 'An invalid parameter was passed to the command.';
    ST_TIMEOUT       : Result := 'The maximum time stipulated for the operation has been exhausted.';
    ST_CANCEL        : Result := 'Operation canceled by the cardholder.';
    ST_MANDAT        : Result := 'A mandatory parameter was not sent by the SPE.';
    ST_TABVERDIF     : Result := 'EMV Tables version differs from the expected.';
    ST_TABERR        : Result := 'Error when trying to write tables (lack of space, for example).';
    ST_INTERR        : Result := 'Internal pinpad error (unexpected situation that does not correspond to the other error codes described here).';
    ST_MCDATAERR     : Result := 'Magnetic card reading error.';
    ST_ERRKEY        : Result := 'MK / DUKPT referenced is not present in the pinpad.';
    ST_NOCARD        : Result := 'There is no ICC present in the coupler or CTLS detected by the antenna.';
    ST_PINBUSY       : Result := 'Pinpad cannot process PIN capture temporarily due to security constrains (such as when the capture limit is reached within a time interval).';
    ST_RSPOVRFL      : Result := 'Response data exceeds the maximum allowed size.';
    ST_ERRCRYPT      : Result := 'Generic cryptographic validation error.';
    ST_DUMBCARD      : Result := 'ICC inserted, but not responding (“mute”).';
    ST_ERRCARD       : Result := 'Communication error between the pinpad and the ICC or CTLS.';
    ST_CARDINVALIDAT : Result := 'ICC is invalidated.';
    ST_CARDPROBLEMS  : Result := 'ICC with problems. This status is valid for many situations in which the ICC does not behave as expected and the transaction must be terminated.';
    ST_CARDINVDATA   : Result := 'The ICC behaves correctly but has invalid or inconsistent data.';
    ST_CARDAPPNAV    : Result := 'ICC with no matching application.';
    ST_CARDAPPNAUT   : Result := 'The application selected in the ICC cannot be used in this situation.';
    ST_ERRFALLBACK   : Result := 'High level error in the ICC that allows fallback to magnetic stripe.';
    ST_INVAMOUNT     : Result := 'Invalid amount for the transaction.';
    ST_ERRMAXAID     : Result := 'Number of candidate AIDs exceeds the processing capacity of the EMV kernel.';
    ST_CARDBLOCKED   : Result := 'Card is blocked.';
    ST_CTLSMULTIPLE  : Result := 'More than one CTLS was presented to the reader simultaneously.';
    ST_CTLSCOMMERR   : Result := 'Communication error between the pinpad (antenna) and the CTLS.';
    ST_CTLSINVALIDAT : Result := 'CTLS is invalidated.';
    ST_CTLSPROBLEMS  : Result := 'CTLS with problems. This status is valid for many situations in which the CTLS does not behave as expected and the transaction must be terminated.';
    ST_CTLSAPPNAV    : Result := 'CTLS with no matching application.';
    ST_CTLSAPPNAUT   : Result := 'The application selected in the CTLS cannot be used in this situation.';
    ST_CTLSEXTCVM    : Result := 'Cardholder must perform a validation on his device (mobile phone, for example) and then re-present it to the pinpad.';
    ST_CTLSIFCHG     : Result := 'CTLS processing resulted in “change interface” (request ICC or magnetic card).';
    ST_MFNFOUND      : Result := 'Media file not found.';
    ST_MFERRFMT      : Result := 'Media file format error.';
    ST_MFERR         : Result := 'Media file loading error.';
  end;
end;

function TACBrAbecsPinPad.GetPort: String;
begin
  Result := Self.Device.Porta;
end;

procedure TACBrAbecsPinPad.SetPort(AValue: String);
begin
  Self.Device.Porta := AValue;
end;

procedure TACBrAbecsPinPad.RegisterLog(const AString: AnsiString;
  Translate: Boolean; AddTime: Boolean);
var
  Done: Boolean;
  s: AnsiString;
begin
  if (Self.LogFile = '') and (not Assigned(Self.OnWriteLog)) then
    Exit;

  if Translate then
    s := TranslateUnprintable(AString)
  else
    s := AString;

  if AddTime then
    s := '-- ' + FormatDateTime('dd/mm hh:nn:ss:zzz', now) + ' - ' + s;

  Done := False;
  if Assigned(Self.OnWriteLog) then
  begin
    Done := True;
    Self.OnWriteLog(s, Done);
  end;

  if (not Done) and (Self.LogFile <> '') then
    WriteToTXT(Self.LogFile, s, True, True, True);
end;

procedure TACBrAbecsPinPad.DoException(AException: Exception);
begin
  if not Assigned(AException) then
    Exit;

  RegisterLog(AException.ClassName+': '+AException.Message);
  raise AException;
end;

procedure TACBrAbecsPinPad.DoException(const AMsg: String);
begin
  DoException( EACBrAbecsPinPadError.Create(AMsg) );
end;

procedure TACBrAbecsPinPad.ExecCommand;
var
  AckByte: Byte;
  ACKFails: Byte;
begin
  RegisterLog('ExecCommand');
  fIsBusy := True;
  try
    // initial cleaning
    AckByte := 0;
    ACKFails := 0;
    fResponse.Clear;

    // Send Data and Wait for ACK
    while (AckByte <> ACK) do
    begin
      SendCommand;
      AckByte := WaitForACK;

      if (AckByte = NAK) then
      begin
        Inc(ACKFails);
        if (ACKFails > MAX_ACK_TRIES) then
          DoException(CERR_READING_ACK);
      end
      else if (AckByte <> ACK) then
        DoException(CERR_READING_ACK);
    end;

    WaitForResponse;
    EvaluateResponse;
  finally
    fIsBusy := False;
  end;
end;

procedure TACBrAbecsPinPad.SendCommand;
var
  Pck: TACBrAbecsPacket;
  s: AnsiString;
begin
  if (Self.LogLevel > 1) then
    RegisterLog('SendCommand');

  if not IsEnabled then
    DoException(CERR_NOTENABLED);

  if Self.IsBusy then
    DoException(CERR_BUSY);

  Pck := TACBrAbecsPacket.Create(fCommand.AsString);
  try
    s := Pck.AsString;
    if (Self.LogLevel > 1) then
      RegisterLog('  '+s);

    Self.Device.EnviaString(s);
  finally
    Pck.Free;
    fCommand.Clear;
  end;
end;

function TACBrAbecsPinPad.SendCAN: Boolean;
var
  b: Byte;
  CanTries: Byte;
begin
  CanTries := 0;
  Result := False;
  repeat
    Inc(CanTries);
    RegisterLog(Format('SendCAN: %d', [CanTries]));
    Self.Device.EnviaByte(CAN);
    try
      b := Self.Device.LeByte(TIMEOUT_ACK);
      Result := (b = EOT);
      if not Result then
        IgnoreAllBytes;
    except
    end;
  until Result or (CanTries >= MAX_ACK_TRIES)
end;

procedure TACBrAbecsPinPad.SendNAK;
begin
  Self.Device.EnviaByte(NAK);
end;

procedure TACBrAbecsPinPad.IgnoreAllBytes(SleepTime: Integer);
begin
  Sleep(SleepTime);
  Self.Device.Limpar;
end;

function TACBrAbecsPinPad.WaitForACK: Byte;
begin
  if (Self.LogLevel > 1) then
    RegisterLog('WaitForACK');

  Result := Self.Device.LeByte(TIMEOUT_ACK);
end;

procedure TACBrAbecsPinPad.WaitForResponse;

  function UserCancelled: Boolean;
  begin
    Result := not IsBusy;
  end;

  procedure WaitForSYN;
  var
    TimeToTimeOut: TDateTime;
    b: Byte;
  begin
    TimeToTimeOut := IncMilliSecond(Now, Self.TimeOut);
    repeat
      if not fCommand.IsBlocking then
      begin
        if (Now > TimeToTimeOut) then
          DoException(CERR_TIMEOUT_RSP);
      end
      else
      begin
        if UserCancelled then
        begin
          if SendCAN then
            DoException(CERR_CANCELLED_BY_USER)
          else
            DoException(CERR_READING_CAN);
        end;
      end;

      try
        b := Self.Device.LeByte(500);
      except
      end;
    until (b = SYN);
  end;

  function WaitForDataPacket: AnsiString;
  var
    b: Byte;
  begin
    Result := '';
    repeat
      b := Self.Device.LeByte(TIMEOUT_ACK);  // Raise Exception for Timeout
      if (b <> ETB) then
      begin
        if (Length(Result) < MAX_PACKET_SIZE) then
          Result := Result + chr(b)
        else
          DoException(CERR_DATAPACKET_TO_LARGE);
      end;
    until (b = ETB)
  end;

var
  b, NumNAKSent: Byte;
  PktData, CRCData: AnsiString;
  Done: Boolean;
  pkt: TACBrAbecsPacket;
begin
  if (Self.LogLevel > 1) then
    RegisterLog('WaitForResponse');

  pkt := TACBrAbecsPacket.Create();
  try
    NumNAKSent := 0;
    PktData := '';
    Done := False;
    repeat
      WaitForSYN;;

      try
        PktData := WaitForDataPacket;
        CRCData := Self.Device.LeString(TIMEOUT_ACK, 2); // Read 2 bytes
        // TACBrAbecsPacket.AsString Setter checks for CRC and raise Exception on error
        pkt.AsString := chr(SYN) + PktData + chr(ETB) + CRCData;
        Done := True;
      except
        IgnoreAllBytes;
        SendNAK;
        PktData := '';
        Inc(NumNAKSent);
        if (NumNAKSent >= MAX_ACK_TRIES) then
          DoException(CERR_READING_RSP);
      end;
    until Done;

    fResponse.AsString := pkt.Data;
  finally
    pkt.Free;
  end;
end;

procedure TACBrAbecsPinPad.EvaluateResponse;
begin
  if (Self.LogLevel > 2) then
    RegisterLog('EvaluateResponse');

  if (fResponse.STAT <> ST_OK) then
    DoException(Format('Error: %d - %s', [fResponse.STAT, ReturnCodeDescription(fResponse.STAT)]));
end;

procedure TACBrAbecsPinPad.CancelWaiting;
begin
  RegisterLog('CancelWaiting');
  fIsBusy := False;
end;

procedure TACBrAbecsPinPad.OpenInsecure;
begin
  RegisterLog('OpenInsecure');
  fCommand.ID := 'OPN';
  ExecCommand;
end;

end.

