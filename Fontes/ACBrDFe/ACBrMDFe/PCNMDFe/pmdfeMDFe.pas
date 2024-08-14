{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pmdfeMDFe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pmdfeConversaoMDFe, pcnSignature, pmdfeProcMDFe, pcnGerador;

type

  TinfMunCarregaCollectionItem = class;
  TinfPercursoCollectionItem   = class;
  TenderEmit                   = class;

  // Informações do modal Rodoviário
  Trodo                      = class;
  Tprop                      = class;
  TveicTracao                = class;
  TcondutorCollection        = class;
  TcondutorCollectionItem    = class;
  TveicReboqueCollection     = class;
  TveicReboqueCollectionItem = class;
  TvalePed                   = class;
  TdispCollection            = class;
  TdispCollectionItem        = class;
  TinfANTT                   = class;
  TinfCIOTCollection         = class;
  TinfCIOTCollectionItem     = class;
  TinfContratanteCollection     = class;
  TinfContratanteCollectionItem = class;

  // Informações do modal Aéreo
  Taereo = class;

  // Informações do modal Aquaviário
  Taquav                           = class;
  TinfTermCarregCollection         = class;
  TinfTermCarregCollectionItem     = class;
  TinfTermDescarregCollection      = class;
  TinfTermDescarregCollectionItem  = class;
  TinfEmbCombCollection            = class;
  TinfEmbCombCollectionItem        = class;
  TinfUnidCargaVaziaCollection     = class;
  TinfUnidCargaVaziaCollectionItem = class;
  TinfUnidTranspVaziaCollection     = class;
  TinfUnidTranspVaziaCollectionItem = class;

  // Informações do modal Ferroviário
  Tferrov            = class;
  TvagCollection     = class;
  TvagCollectionItem = class;

  TinfDoc                       = class;
  TinfMunDescargaCollection     = class;
  TinfMunDescargaCollectionItem = class;

  TPeriCTeCollection            = class;

  TinfCTeCollection           = class;
  TinfCTeCollectionItem       = class;
  TinfUnidTranspCTeCollection = class;

  TinfUnidTranspCollectionItem = class;
  TinfUnidCargaCollection      = class;
  TinfUnidCargaCollectionItem  = class;

  TinfCTCollection           = class;
  TinfCTCollectionItem       = class;

  TinfNFeCollection           = class;
  TinfNFeCollectionItem       = class;

  TinfNFCollection           = class;
  TinfNFCollectionItem       = class;

  TinfMDFeTranspCollection     = class;
  TinfMDFeTranspCollectionItem = class;

  TSegCollection     = class;
  TSegCollectionItem = class;

  TAverCollection     = class;
  TAverCollectionItem = class;

  Ttot                  = class;
  TlacresCollection     = class;
  TlacresCollectionItem = class;
  TautXMLCollection     = class;
  TautXMLCollectionItem = class;
  TinfAdic              = class;

  TMDFe = class;

  TinfMDFe = class(TObject)
  private
    FId: string;
    FVersao: Double;
//    function GetVersaoStr: string;
  public
    property Id: string read FId write FId;
    property versao: Double read FVersao write FVersao;
//    property VersaoStr: string read GetVersaoStr;
  end;

  TinfMunCarregaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfMunCarregaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMunCarregaCollectionItem);
  public
    function Add: TinfMunCarregaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfMunCarregaCollectionItem;
    property Items[Index: Integer]: TinfMunCarregaCollectionItem read GetItem write SetItem; default;
  end;

  TinfMunCarregaCollectionItem = class(TObject)
  private
    FcMunCarrega: Integer;
    FxMunCarrega: string;
  public
    property cMunCarrega: Integer read FcMunCarrega write FcMunCarrega;
    property xMunCarrega: string  read FxMunCarrega write FxMunCarrega;
  end;

  TinfPercursoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfPercursoCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfPercursoCollectionItem);
  public
    function Add: TinfPercursoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfPercursoCollectionItem;
    property Items[Index: Integer]: TinfPercursoCollectionItem read GetItem write SetItem; default;
  end;

  TinfPercursoCollectionItem = class(TObject)
  private
    FUFPer: string;
  public
    property UFPer: string read FUFPer write FUFPer;
  end;

  TIde = class(TObject)
  private
    FcUF: Integer;
    FtpAmb: TpcnTipoAmbiente;
    FtpEmit: TTpEmitenteMDFe;
    FtpTransp: TTransportadorMDFe;
    Fmod: string;
    Fserie: Integer;
    FnMDF: Integer;
    FcMDF: Integer;
    FcDV: Integer;
    Fmodal: TModalMDFe;
    FdhEmi: TDateTime;
    FtpEmis: TpcnTipoEmissao;
    FprocEmi: TpcnProcessoEmissao;
    FverProc: string;
    FUFIni: string;
    FUFFim: string;
    FinfMunCarrega: TinfMunCarregaCollection;
    FinfPercurso: TinfPercursoCollection;
    FdhIniViagem: TDateTime;
    FindCanalVerde: TIndicador;
    FindCarregaPosterior: TIndicador;

    procedure SetinfMunCarrega(Value: TinfMunCarregaCollection);
    procedure SetinfPercurso(Value: TinfPercursoCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property cUF: Integer                            read FcUF           write FcUF;
    property tpAmb: TpcnTipoAmbiente                 read FtpAmb         write FtpAmb;
    property tpEmit: TTpEmitenteMDFe                 read FtpEmit        write FtpEmit;
    property tpTransp: TTransportadorMDFe            read FtpTransp      write FtpTransp;
    property modelo: string                          read Fmod           write Fmod;
    property serie: Integer                          read Fserie         write Fserie;
    property nMDF: Integer                           read FnMDF          write FnMDF;
    property cMDF: Integer                           read FcMDF          write FcMDF;
    property cDV: Integer                            read FcDV           write FcDV;
    property modal: TModalMDFe                       read Fmodal         write Fmodal;
    property dhEmi: TDateTime                        read FdhEmi         write FdhEmi;
    property tpEmis: TpcnTipoEmissao                 read FtpEmis        write FtpEmis;
    property procEmi: TpcnProcessoEmissao            read FprocEmi       write FprocEmi;
    property verProc: string                         read FverProc       write FverProc;
    property UFIni: string                           read FUFIni         write FUFIni;
    property UFFim: string                           read FUFFim         write FUFFim;
    property infMunCarrega: TinfMunCarregaCollection read FinfMunCarrega write SetinfMunCarrega;
    property infPercurso: TinfPercursoCollection     read FinfPercurso   write SetinfPercurso;
    property dhIniViagem: TDateTime                  read FdhIniViagem   write FdhIniViagem;
    property indCanalVerde: TIndicador               read FindCanalVerde write FindCanalVerde default tiNao;
    property indCarregaPosterior: TIndicador         read FindCarregaPosterior write FindCarregaPosterior default tiNao;
  end;

  Temit = class(TObject)
  private
    FCNPJCPF: string;
    FIE: string;
    FxNome: string;
    FxFant: string;
    FenderEmit: TenderEmit;
  public
    constructor Create;
    destructor Destroy; override;
    property CNPJCPF: string       read FCNPJCPF   write FCNPJCPF;
    property IE: string            read FIE        write FIE;
    property xNome: string         read FxNome     write FxNome;
    property xFant: string         read FxFant     write FxFant;
    property enderEmit: TenderEmit read FenderEmit write FenderEmit;
  end;

  TenderEmit = class(TObject)
  private
    FxLgr: string;
    Fnro: string;
    FxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FCEP: Integer;
    FUF: string;
    Ffone: string;
    Femail: string;
  public
    property xLgr: string    read FxLgr    write FxLgr;
    property nro: string     read Fnro     write Fnro;
    property xCpl: string    read FxCpl    write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: Integer   read FcMun    write FcMun;
    property xMun: string    read FxMun    write FxMun;
    property CEP: Integer    read FCEP     write FCEP;
    property UF: string      read FUF      write FUF;
    property fone: string    read Ffone    write Ffone;
    property email: string   read Femail   write Femail;
  end;

  TlacresCollectionItem = class(TObject)
  private
    FnLacre: string;
  public
    property nLacre: string read FnLacre write FnLacre;
  end;

  TlacresCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TlacresCollectionItem;
    procedure SetItem(Index: Integer; Value: TlacresCollectionItem);
  public
    function Add: TlacresCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TlacresCollectionItem;
    property Items[Index: Integer]: TlacresCollectionItem read GetItem write SetItem; default;
  end;

  TlacRodoCollection       = TlacresCollection;
  TlacUnidTranspCollection = TlacresCollection;
  TlacUnidCargaCollection  = TlacresCollection;

  TRodo = class(TObject)
  private
    FRNTRC: string;
    FCIOT: string;
    FinfANTT: TinfANTT;
    FveicTracao: TveicTracao;
    FveicReboque: TveicReboqueCollection;
    FvalePed: TvalePed;
    FcodAgPorto: string;
    FlacRodo: TlacRodoCollection;

    procedure SetveicReboque(const Value: TveicReboqueCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property RNTRC: string                       read FRNTRC       write FRNTRC;
    property CIOT: string                        read FCIOT        write FCIOT;
    property infANTT: TinfANTT                   read FinfANTT     write FinfANTT;
    property veicTracao: TveicTracao             read FveicTracao  write FveicTracao;
    property veicReboque: TveicReboqueCollection read FveicReboque write SetveicReboque;
    property valePed: TvalePed                   read FvalePed     write FvalePed;
    property codAgPorto: string                  read FcodAgPorto  write FcodAgPorto;
    property lacRodo: TlacRodoCollection         read FlacRodo     write FlacRodo;
  end;

  TCompCollectionItem = class(TObject)
  private
    FtpComp: TComp;
    FvComp: Double;
    FxComp: string;
  public
    property tpComp: TComp read FtpComp write FtpComp;
    property vComp: Double read FvComp  write FvComp;
    property xComp: string read FxComp  write FxComp;
  end;

  TCompCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TCompCollectionItem;
    procedure SetItem(Index: Integer; Value: TCompCollectionItem);
  public
    function Add: TCompCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TCompCollectionItem;
    property Items[Index: Integer]: TCompCollectionItem read GetItem write SetItem; default;
  end;

  TInfPrazoCollectionItem = class(TObject)
  private
    FnParcela: Integer;
    FdVenc: TDateTime;
    FvParcela: Double;
  public
    property nParcela: Integer  read FnParcela   write FnParcela;
    property dVenc: TDateTime   read FdVenc      write FdVenc;
    property vParcela: Double   read FvParcela   write FvParcela;
  end;

  TInfPrazoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfPrazoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfPrazoCollectionItem);
  public
    function Add: TInfPrazoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfPrazoCollectionItem;
    property Items[Index: Integer]: TInfPrazoCollectionItem read GetItem write SetItem; default;
  end;

  TinfBanc = class(TObject)
  private
    FcodBanco: string;
    FcodAgencia: string;
    FCNPJIPEF: string;
    FPIX: string;
  public
    property codBanco: string   read FcodBanco   write FcodBanco;
    property codAgencia: string read FcodAgencia write FcodAgencia;
    property CNPJIPEF: string   read FCNPJIPEF   write FCNPJIPEF;
    property PIX: string        read FPIX        write FPIX;
  end;

  TinfPagCollectionItem = class(TObject)
  private
    FxNome: string;
    FCNPJCPF: string;
    FidEstrangeiro: string;
    FComp: TCompCollection;
    FvContrato: Double;
    FindAltoDesemp: TIndicador;
    FindPag: TIndPag;
    FvAdiant: Double;
    FindAntecipaAdiant: TIndicador;
    FinfPrazo: TInfPrazoCollection;
    FtpAntecip: TtpAntecip;
    FinfBanc: TinfBanc;
  public
    constructor Create;
    destructor Destroy; override;

    property xNome: string                 read FxNome         write FxNome;
    property CNPJCPF: string               read FCNPJCPF       write FCNPJCPF;
    property idEstrangeiro: string         read FidEstrangeiro write FidEstrangeiro;
    property Comp: TCompCollection         read FComp          write FComp;
    property vContrato: Double             read FvContrato     write FvContrato;
    property indAltoDesemp: TIndicador     read FindAltoDesemp write FindAltoDesemp;
    property indPag: TIndPag               read FindPag        write FindPag;
    property vAdiant: Double               read FvAdiant       write FvAdiant;
    property indAntecipaAdiant: TIndicador read FindAntecipaAdiant write FindAntecipaAdiant default tiNao;
    property infPrazo: TInfPrazoCollection read FinfPrazo      write FinfPrazo;
    property tpAntecip: TtpAntecip         read FtpAntecip     write FtpAntecip;
    property infBanc: TinfBanc             read FinfBanc       write FinfBanc;
  end;

  TinfPagCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfPagCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfPagCollectionItem);
  public
    function Add: TinfPagCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfPagCollectionItem;
    property Items[Index: Integer]: TinfPagCollectionItem read GetItem write SetItem; default;
  end;

  TinfANTT    = class(TObject)
  private
    FRNTRC: string;
    FinfCIOT: TinfCIOTCollection;
    FinfContratante: TinfContratanteCollection;
    FvalePed: TvalePed;
    FinfPag: TinfPagCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property RNTRC: string                             read FRNTRC          write FRNTRC;
    property infCIOT: TinfCIOTCollection               read FinfCIOT        write FinfCIOT;
    property infContratante: TinfContratanteCollection read FinfContratante write FinfContratante;
    property valePed: TvalePed                         read FvalePed        write FvalePed;
    property infPag: TinfPagCollection                 read FinfPag         write FinfPag;
    (*
    property cInt: string                  read FcInt     write FcInt;
    property placa: string                 read Fplaca    write Fplaca;
    property RENAVAM: string               read FRENAVAM  write FRENAVAM;
    property tara: Integer                 read Ftara     write Ftara;
    property capKG: Integer                read FcapKG    write FcapKG;
    property capM3: Integer                read FcapM3    write FcapM3;
    property prop: Tprop                   read Fprop     write Fprop;
    property condutor: TcondutorCollection read Fcondutor write Setcondutor;
    property tpRod: TpcteTipoRodado        read FtpRod    write FtpRod;
    property tpCar: TpcteTipoCarroceria    read FtpCar    write FtpCar;
    property UF: string                    read FUF       write FUF;
    *)
  end;

  TinfCIOTCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfCIOTCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfCIOTCollectionItem);
  public
    function Add: TinfCIOTCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfCIOTCollectionItem;
    property Items[Index: Integer]: TinfCIOTCollectionItem read GetItem write SetItem; default;
  end;

  TinfCIOTCollectionItem = class(TObject)
  private
    FCIOT: string;
    FCNPJCPF: string;
  public
    property CIOT: string    read FCIOT    write FCIOT;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
  end;

  TinfContratanteCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfContratanteCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfContratanteCollectionItem);
  public
    function Add: TinfContratanteCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfContratanteCollectionItem;
    property Items[Index: Integer]: TinfContratanteCollectionItem read GetItem write SetItem; default;
  end;

  TinfContrato = class(TObject)
  private
    FNroContrato: string;
    FvContratoGlobal: Double;
  public
    property NroContrato: string read FNroContrato write FNroContrato;
    property vContratoGlobal: Double read FvContratoGlobal write FvContratoGlobal;
  end;

  TinfContratanteCollectionItem = class(TObject)
  private
    FxNome: string;
    FCNPJCPF: string;
    FidEstrangeiro: string;
    FinfContrato: TinfContrato;
  public
    constructor Create;
    destructor Destroy; override;

    property xNome: string         read FxNome         write FxNome;
    property CNPJCPF: string       read FCNPJCPF       write FCNPJCPF;
    property idEstrangeiro: string read FidEstrangeiro write FidEstrangeiro;
    property infContrato: TinfContrato read FinfContrato write FinfContrato;
  end;

  TveicTracao = class(TObject)
  private
    FcInt: string;
    Fplaca: string;
    FRENAVAM: string;
    Ftara: Integer;
    FcapKG: Integer;
    FcapM3: Integer;
    Fprop: Tprop;
    Fcondutor: TcondutorCollection;
    FtpRod: TpcteTipoRodado;
    FtpCar: TpcteTipoCarroceria;
    FUF: string;

    procedure Setcondutor(const Value: TcondutorCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property cInt: string                  read FcInt     write FcInt;
    property placa: string                 read Fplaca    write Fplaca;
    property RENAVAM: string               read FRENAVAM  write FRENAVAM;
    property tara: Integer                 read Ftara     write Ftara;
    property capKG: Integer                read FcapKG    write FcapKG;
    property capM3: Integer                read FcapM3    write FcapM3;
    property prop: Tprop                   read Fprop     write Fprop;
    property condutor: TcondutorCollection read Fcondutor write Setcondutor;
    property tpRod: TpcteTipoRodado        read FtpRod    write FtpRod;
    property tpCar: TpcteTipoCarroceria    read FtpCar    write FtpCar;
    property UF: string                    read FUF       write FUF;
  end;

  Tprop = class(TObject)
  private
    FCNPJCPF: string;
    FRNTRC: string;
    FxNome: string;
    FIE: string;
    FUF: string;
    FtpProp: TpcteProp;
  public
    property CNPJCPF: string   read FCNPJCPF write FCNPJCPF;
    property RNTRC: string     read FRNTRC   write FRNTRC;
    property xNome: string     read FxNome   write FxNome;
    property IE: string        read FIE      write FIE;
    property UF: string        read FUF      write FUF;
    property tpProp: TpcteProp read FtpProp  write FtpProp;
  end;

  TcondutorCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TcondutorCollectionItem;
    procedure SetItem(Index: Integer; Value: TcondutorCollectionItem);
  public
    function Add: TcondutorCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TcondutorCollectionItem;
    property Items[Index: Integer]: TcondutorCollectionItem read GetItem write SetItem; default;
  end;

  TcondutorCollectionItem = class(TObject)
  private
    FxNome: string;
    FCPF: string;
  public
    property xNome: string read FxNome write FxNome;
    property CPF: string   read FCPF   write FCPF;
  end;

  TveicReboqueCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TveicReboqueCollectionItem;
    procedure SetItem(Index: Integer; Value: TveicReboqueCollectionItem);
  public
    function Add: TveicReboqueCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TveicReboqueCollectionItem;
    property Items[Index: Integer]: TveicReboqueCollectionItem read GetItem write SetItem; default;
  end;

  TveicReboqueCollectionItem = class(TObject)
  private
    FcInt: string;
    Fplaca: string;
    FRENAVAM: string;
    Ftara: Integer;
    FcapKG: Integer;
    FcapM3: Integer;
    Fprop: Tprop;
    FtpCar: TpcteTipoCarroceria;
    FUF: string;
  public
    constructor Create;
    destructor Destroy; override;
    property cInt: string               read FcInt    write FcInt;
    property placa: string              read Fplaca   write Fplaca;
    property RENAVAM: string            read FRENAVAM write FRENAVAM;
    property tara: Integer              read Ftara    write Ftara;
    property capKG: Integer             read FcapKG   write FcapKG;
    property capM3: Integer             read FcapM3   write FcapM3;
    property prop: Tprop                read Fprop    write Fprop;
    property tpCar: TpcteTipoCarroceria read FtpCar   write FtpCar;
    property UF: string                 read FUF      write FUF;
  end;

  TvalePed = class(TObject)
  private
    Fdisp: TdispCollection;
    FcategCombVeic: TcategCombVeic;

    procedure Setdisp(const Value: TdispCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property disp: TdispCollection read Fdisp write Setdisp;
    property categCombVeic: TcategCombVeic read FcategCombVeic write FcategCombVeic;
  end;

  TdispCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdispCollectionItem;
    procedure SetItem(Index: Integer; Value: TdispCollectionItem);
  public
    function Add: TdispCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdispCollectionItem;
    property Items[Index: Integer]: TdispCollectionItem read GetItem write SetItem; default;
  end;

  TdispCollectionItem = class(TObject)
  private
    FCNPJForn: string;
    FCNPJPg: string;
    FnCompra: string;
    FvValePed: Double;
    FtpValePed: TtpValePed;
  public
    property CNPJForn: string read FCNPJForn write FCNPJForn;
    property CNPJPg: string   read FCNPJPg   write FCNPJPg;
    property nCompra: string  read FnCompra  write FnCompra;
    property vValePed: Double read FvValePed write FvValePed;
    property tpValePed: TtpValePed read FtpValePed write FtpValePed;
  end;

  Taereo = class(TObject)
  private
    Fnac: string;
    Fmatr: string;
    FnVoo: string;
    FcAerEmb: string;
    FcAerDes: string;
    FdVoo: TDateTime;
  public
    property nac: string     read Fnac     write Fnac;
    property matr: string    read Fmatr    write Fmatr;
    property nVoo: string    read FnVoo    write FnVoo;
    property cAerEmb: string read FcAerEmb write FcAerEmb;
    property cAerDes: string read FcAerDes write FcAerDes;
    property dVoo: TDateTime read FdVoo    write FdVoo;
  end;

  Taquav = class(TObject)
  private
    FCNPJAgeNav: string;
    Firin : string;
    FtpEmb: string;
    FcEmbar: string;
    FxEmbar: string;
    FnViagem: string;
    FcPrtEmb: string;
    FcPrtDest: string;
    FprtTrans : string;
    FtpNav    : TTipoNavegacao;
    FinfTermCarreg: TinfTermCarregCollection;
    FinfTermDescarreg: TinfTermDescarregCollection;
    FinfEmbComb: TinfEmbCombCollection;
    FinfUnidCargaVazia: TinfUnidCargaVaziaCollection;
    FinfUnidTranspVazia: TinfUnidTranspVaziaCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property CNPJAgeNav: string                              read FCNPJAgeNav        write FCNPJAgeNav;
    property irin: string                                    read Firin              write Firin;
    property tpEmb: string                                   read FtpEmb             write FtpEmb;
    property cEmbar: string                                  read FcEmbar            write FcEmbar;
    property xEmbar: string                                  read FxEmbar            write FxEmbar;
    property nViagem: string                                 read FnViagem           write FnViagem;
    property cPrtEmb: string                                 read FcPrtEmb           write FcPrtEmb;
    property cPrtDest: string                                read FcPrtDest          write FcPrtDest;
    property prtTrans: string                                read FprtTrans          write FprtTrans;
    property tpNav: TTipoNavegacao                           read FtpNav             write FtpNav;
    property infTermCarreg: TinfTermCarregCollection         read FinfTermCarreg     write FinfTermCarreg;
    property infTermDescarreg: TinfTermDescarregCollection   read FinfTermDescarreg  write FinfTermDescarreg;
    property infEmbComb: TinfEmbCombCollection               read FinfEmbComb        write FinfEmbComb;
    property infUnidCargaVazia: TinfUnidCargaVaziaCollection read FinfUnidCargaVazia write FinfUnidCargaVazia;
    property infUnidTranspVazia: TinfUnidTranspVaziaCollection read FinfUnidTranspVazia write FinfUnidTranspVazia;
  end;

  TinfTermCarregCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfTermCarregCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfTermCarregCollectionItem);
  public
    function Add: TinfTermCarregCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfTermCarregCollectionItem;
    property Items[Index: Integer]: TinfTermCarregCollectionItem read GetItem write SetItem; default;
  end;

  TinfTermCarregCollectionItem = class(TObject)
  private
    FcTermCarreg: string;
    FxTermCarreg: string;
  public
    property cTermCarreg: string read FcTermCarreg write FcTermCarreg;
    property xTermCarreg: string read FxTermCarreg write FxTermCarreg;
  end;

  TinfTermDescarregCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfTermDescarregCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfTermDescarregCollectionItem);
  public
    function Add: TinfTermDescarregCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfTermDescarregCollectionItem;
    property Items[Index: Integer]: TinfTermDescarregCollectionItem read GetItem write SetItem; default;
  end;

  TinfTermDescarregCollectionItem = class(TObject)
  private
    FcTermDescarreg: string;
    FxTermDescarreg: string;
  public
    property cTermDescarreg: string read FcTermDescarreg write FcTermDescarreg;
    property xTermDescarreg: string read FxTermDescarreg write FxTermDescarreg;
  end;

  TinfEmbCombCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfEmbCombCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfEmbCombCollectionItem);
  public
    function Add: TinfEmbCombCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfEmbCombCollectionItem;
    property Items[Index: Integer]: TinfEmbCombCollectionItem read GetItem write SetItem; default;
  end;

  TinfEmbCombCollectionItem = class(TObject)
  private
    FcEmbComb: string;
    FxBalsa  : string;
  public
    property cEmbComb: string read FcEmbComb write FcEmbComb;
    property xBalsa: string read FxBalsa write FxBalsa;
  end;

  TinfUnidCargaVaziaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfUnidCargaVaziaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidCargaVaziaCollectionItem);
  public
    function Add: TinfUnidCargaVaziaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfUnidCargaVaziaCollectionItem;
    property Items[Index: Integer]: TinfUnidCargaVaziaCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidCargaVaziaCollectionItem = class(TObject)
  private
    FidUnidCargaVazia: string;
    FtpUnidCargaVazia: TpcnUnidCarga;
  public
    property idUnidCargaVazia: string        read FidUnidCargaVazia write FidUnidCargaVazia;
    property tpUnidCargaVazia: TpcnUnidCarga read FtpUnidCargaVazia write FtpUnidCargaVazia;
  end;

  TinfUnidTranspVaziaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfUnidTranspVaziaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspVaziaCollectionItem);
  public
    function Add: TinfUnidTranspVaziaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfUnidTranspVaziaCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspVaziaCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidTranspVaziaCollectionItem = class(TObject)
  private
    FidUnidTranspVazia: string;
    FtpUnidTranspVazia: TpcnUnidTransp;
  public
    property idUnidTranspVazia: string        read FidUnidTranspVazia write FidUnidTranspVazia;
    property tpUnidTranspVazia: TpcnUnidTransp read FtpUnidTranspVazia write FtpUnidTranspVazia;
  end;

  Tferrov = class(TObject)
  private
    FxPref: string;
    FdhTrem: TDateTime;
    FxOri: string;
    FxDest: string;
    FqVag: Integer;
    Fvag: TvagCollection;

    procedure Setvag(const Value: TvagCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property xPref: string       read FxPref  write FxPref;
    property dhTrem: TDateTime   read FdhTrem write FdhTrem;
    property xOri: string        read FxOri   write FxOri;
    property xDest: string       read FxDest  write FxDest;
    property qVag: Integer       read FqVag   write FqVag;
    property vag: TvagCollection read Fvag    write Setvag;
  end;

  TvagCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TvagCollectionItem;
    procedure SetItem(Index: Integer; Value: TvagCollectionItem);
  public
    function Add: TvagCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TvagCollectionItem;
    property Items[Index: Integer]: TvagCollectionItem read GetItem write SetItem; default;
  end;

  TvagCollectionItem = class(TObject)
  private
    FpesoBC: Double;
    FpesoR: Double;
    FtpVag: string;
    Fserie: string;
    FnVag: Integer;
    FnSeq: Integer;
    FTU: Double;
  public
    property pesoBC: Double read FpesoBC write FpesoBC;
    property pesoR: Double  read FpesoR  write FpesoR;
    property tpVag: string  read FtpVag  write FtpVag;
    property serie: string  read Fserie  write Fserie;
    property nVag: Integer  read FnVag   write FnVag;
    property nSeq: Integer  read FnSeq   write FnSeq;
    property TU: Double     read FTU     write FTU;
  end;

  TPeriCollectionItem = class(TObject)
  private
    FnONU        : string;
    FxNomeAE     : string;
    FxClaRisco   : string;
    FgrEmb       : string;
    FqTotProd    : string;
    FqVolTipo    : string;
  public
    property nONU: string        read FnONU        write FnONU;
    property xNomeAE: string     read FxNomeAE     write FxNomeAE;
    property xClaRisco: string   read FxClaRisco   write FxClaRisco;
    property grEmb: string       read FgrEmb       write FgrEmb;
    property qTotProd: string    read FqTotProd    write FqTotProd;
    property qVolTipo: string    read FqVolTipo    write FqVolTipo;
  end;

  TPeriCTeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TPeriCollectionItem;
    procedure SetItem(Index: Integer; Value: TPeriCollectionItem);
  public
    function Add: TPeriCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TPeriCollectionItem;
    property Items[Index: Integer]: TPeriCollectionItem read GetItem write SetItem; default;
  end;

  TPeriNFeCollection  = TPeriCTeCollection;
  TPeriMDFeCollection = TPeriCTeCollection;

  TinfDoc = class(TObject)
  private
    FinfMunDescarga: TinfMunDescargaCollection;
    procedure SetinfMunDescarga(const Value: TinfMunDescargaCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property infMunDescarga: TinfMunDescargaCollection read FinfMunDescarga write SetinfMunDescarga;
  end;

  TinfMunDescargaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfMunDescargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMunDescargaCollectionItem);
  public
    function Add: TinfMunDescargaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfMunDescargaCollectionItem;
    property Items[Index: Integer]: TinfMunDescargaCollectionItem read GetItem write SetItem; default;
  end;

  TinfMunDescargaCollectionItem = class(TObject)
  private
    FcMunDescarga: Integer;
    FxMunDescarga: string;
    FinfCTe: TinfCTeCollection;
    FinfCT: TinfCTCollection;
    FinfNFe: TinfNFeCollection;
    FinfNF: TinfNFCollection;
    FinfMDFeTransp: TinfMDFeTranspCollection;

    procedure SetinfCTe(const Value: TinfCTeCollection);
    procedure SetinfCT(const Value: TinfCTCollection);
    procedure SetinfNFe(const Value: TinfNFeCollection);
    procedure SetinfNF(const Value: TinfNFCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property cMunDescarga: Integer                   read FcMunDescarga  write FcMunDescarga;
    property xMunDescarga: string                    read FxMunDescarga  write FxMunDescarga;
    property infCTe: TinfCTeCollection               read FinfCTe        write SetinfCTe;
    property infCT: TinfCTCollection                 read FinfCT         write SetinfCT;
    property infNFe: TinfNFeCollection               read FinfNFe        write SetinfNFe;
    property infNF: TinfNFCollection                 read FinfNF         write SetinfNF;
    property infMDFeTransp: TinfMDFeTranspCollection read FinfMDFeTransp write FinfMDFeTransp;
  end;

  TinfEntregaParcial = class(TObject)
  private
    FqtdTotal: Double;
    FqtdParcial: Double;
  public
    property qtdTotal: Double read FqtdTotal write FqtdTotal;
    property qtdParcial: Double read FqtdParcial write FqtdParcial;
  end;

  TinfNFePrestParcialCollectionItem = class(TObject)
  private
    FchNFe: string;
  public
    property chNFe: string read FchNFe write FchNFe;
  end;

  TinfNFePrestParcialCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfNFePrestParcialCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfNFePrestParcialCollectionItem);
  public
    function Add: TinfNFePrestParcialCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfNFePrestParcialCollectionItem;
    property Items[Index: Integer]: TinfNFePrestParcialCollectionItem read GetItem write SetItem; default;
  end;

  TinfCTeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfCTeCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfCTeCollectionItem);
  public
    function Add: TinfCTeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfCTeCollectionItem;
    property Items[Index: Integer]: TinfCTeCollectionItem read GetItem write SetItem; default;
  end;

  TinfCTeCollectionItem = class(TObject)
  private
    FchCTe: string;
    FSegCodBarra: string;
    FindReentrega: string;
    FinfUnidTransp: TinfUnidTranspCTeCollection;
    Fperi: TPeriCTeCollection;
    FinfEntregaParcial: TinfEntregaParcial;
    FindPrestacaoParcial: TIndicadorEx;
    FinfNFePrestParcial: TinfNFePrestParcialCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property chCTe: string                              read FchCTe               write FchCTe;
    property SegCodBarra: string                        read FSegCodBarra         write FSegCodBarra;
    property indReentrega: string                       read FindReentrega        write FindReentrega;
    property infUnidTransp: TinfUnidTranspCTeCollection read FinfUnidTransp       write FinfUnidTransp;
    property peri: TPeriCTeCollection                   read Fperi                write Fperi;
    property infEntregaParcial: TinfEntregaParcial      read FinfEntregaParcial   write FinfEntregaParcial;
    property indPrestacaoParcial: TIndicadorEx          read FindPrestacaoParcial write FindPrestacaoParcial default tieNenhum;

    property infNFePrestParcial: TinfNFePrestParcialCollection read FinfNFePrestParcial  write FinfNFePrestParcial;
  end;

  TinfUnidTranspCollectionItem = class(TObject)
  private
    FtpUnidTransp: TpcnUnidTransp;
    FidUnidTransp: string;
    FlacUnidTransp: TlacUnidTranspCollection;
    FinfUnidCarga: TinfUnidCargaCollection;
    FqtdRat: Double;
  public
    constructor Create;
    destructor Destroy; override;
    property tpUnidTransp: TpcnUnidTransp            read FtpUnidTransp  write FtpUnidTransp;
    property idUnidTransp: string                    read FidUnidTransp  write FidUnidTransp;
    property lacUnidTransp: TlacUnidTranspCollection read FlacUnidTransp write FlacUnidTransp;
    property infUnidCarga: TinfUnidCargaCollection   read FinfUnidCarga  write FinfUnidCarga;
    property qtdRat: Double                          read FqtdRat        write FqtdRat;
  end;

  TinfUnidTranspCTeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfUnidTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidTranspCollectionItem);
  public
    function Add: TinfUnidTranspCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfUnidTranspCollectionItem;
    property Items[Index: Integer]: TinfUnidTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidTranspCTCollection   = TinfUnidTranspCTeCollection;
  TinfUnidTranspNFeCollection  = TinfUnidTranspCTeCollection;
  TinfUnidTranspNFCollection   = TinfUnidTranspCTeCollection;
  TinfUnidTranspMDFeCollection = TinfUnidTranspCTeCollection;

  TinfUnidCargaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfUnidCargaCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfUnidCargaCollectionItem);
  public
    function Add: TinfUnidCargaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfUnidCargaCollectionItem;
    property Items[Index: Integer]: TinfUnidCargaCollectionItem read GetItem write SetItem; default;
  end;

  TinfUnidCargaCollectionItem = class(TObject)
  private
    FtpUnidCarga: TpcnUnidCarga;
    FidUnidCarga: string;
    FlacUnidCarga: TlacUnidCargaCollection;
    FqtdRat: Double;
  public
    constructor Create;
    destructor Destroy; override;
    property tpUnidCarga: TpcnUnidCarga            read FtpUnidCarga  write FtpUnidCarga;
    property idUnidCarga: string                   read FidUnidCarga  write FidUnidCarga;
    property lacUnidCarga: TlacUnidCargaCollection read FlacUnidCarga write FlacUnidCarga;
    property qtdRat: Double                        read FqtdRat       write FqtdRat;
  end;

  TinfCTCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfCTCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfCTCollectionItem);
  public
    function Add: TinfCTCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfCTCollectionItem;
    property Items[Index: Integer]: TinfCTCollectionItem read GetItem write SetItem; default;
  end;

  TinfCTCollectionItem = class(TObject)
  private
    FnCT: string;
    Fserie: Integer;
    Fsubser: Integer;
    FdEmi: TDateTime;
    FvCarga: Double;
    FinfUnidTransp: TinfUnidTranspCTCollection;

  public
    constructor Create;
    destructor Destroy; override;
    property nCT: string                               read FnCT           write FnCT;
    property serie: Integer                            read Fserie         write Fserie;
    property subser: Integer                           read Fsubser        write Fsubser;
    property dEmi: TDateTime                           read FdEmi          write FdEmi;
    property vCarga: Double                            read FvCarga        write FvCarga;
    property infUnidTransp: TinfUnidTranspCTCollection read FinfUnidTransp write FinfUnidTransp;
  end;

  TinfNFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfNFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfNFeCollectionItem);
  public
    function Add: TinfNFeCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfNFeCollectionItem;
    property Items[Index: Integer]: TinfNFeCollectionItem read GetItem write SetItem; default;
  end;

  TinfNFeCollectionItem = class(TObject)
  private
    FchNFe: string;
    FSegCodBarra: string;
    FindReentrega: string;
    FinfUnidTransp: TinfUnidTranspNFeCollection;
    Fperi: TPeriNFeCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspNFeCollection);
    procedure SetPeri(const Value: TPeriNFeCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property chNFe: string                              read FchNFe         write FchNFe;
    property SegCodBarra: string                        read FSegCodBarra   write FSegCodBarra;
    property indReentrega: string                       read FindReentrega  write FindReentrega;
    property infUnidTransp: TinfUnidTranspNFeCollection read FinfUnidTransp write SetinfUnidTransp;
    property peri: TPeriNFeCollection                   read Fperi          write SetPeri;
  end;

  TinfNFCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfNFCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfNFCollectionItem);
  public
    function Add: TinfNFCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfNFCollectionItem;
    property Items[Index: Integer]: TinfNFCollectionItem read GetItem write SetItem; default;
  end;

  TinfNFCollectionItem = class(TObject)
  private
    FCNPJ: string;
    FUF: string;
    FnNF: Integer;
    Fserie: Integer;
    FdEmi: TDateTime;
    FvNF: Double;
    FPIN: Integer;
    FinfUnidTransp: TinfUnidTranspNFCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property CNPJ: string                              read FCNPJ          write FCNPJ;
    property UF: string                                read FUF            write FUF;
    property nNF: Integer                              read FnNF           write FnNF;
    property serie: Integer                            read Fserie         write Fserie;
    property dEmi: TDateTime                           read FdEmi          write FdEmi;
    property vNF: Double                               read FvNF           write FvNF;
    property PIN: Integer                              read FPIN           write FPIN;
    property infUnidTransp: TinfUnidTranspNFCollection read FinfUnidTransp write FinfUnidTransp;
  end;

  TinfMDFeTranspCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfMDFeTranspCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMDFeTranspCollectionItem);
  public
    function Add: TinfMDFeTranspCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfMDFeTranspCollectionItem;
    property Items[Index: Integer]: TinfMDFeTranspCollectionItem read GetItem write SetItem; default;
  end;

  TinfMDFeTranspCollectionItem = class(TObject)
  private
    FchMDFe: string;
    FindReentrega: string;
    FinfUnidTransp: TinfUnidTranspMDFeCollection;
    Fperi: TPeriMDFeCollection;

    procedure SetinfUnidTransp(const Value: TinfUnidTranspMDFeCollection);
    procedure SetPeri(const Value: TPeriMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;
    property chMDFe: string                              read FchMDFe        write FchMDFe;
    property indReentrega: string                        read FindReentrega  write FindReentrega;
    property infUnidTransp: TinfUnidTranspMDFeCollection read FinfUnidTransp write SetinfUnidTransp;
    property peri: TPeriMDFeCollection                   read Fperi          write SetPeri;
  end;

  TSegCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TSegCollectionItem;
    procedure SetItem(Index: Integer; Value: TSegCollectionItem);
  public
    function Add: TSegCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TSegCollectionItem;
    property Items[Index: Integer]: TSegCollectionItem read GetItem write SetItem; default;
  end;

  TSegCollectionItem = class(TObject)
  private
    FrespSeg: TRspSegMDFe;
    FCNPJCPF: string;
    FxSeg: string;
    FCNPJ: string;
    FnApol: string;
    FAver: TAverCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property respSeg: TRspSegMDFe read FrespSeg write FrespSeg;
    property CNPJCPF: string      read FCNPJCPF write FCNPJCPF;
    property xSeg: string         read FxSeg    write FxSeg;
    property CNPJ: string         read FCNPJ    write FCNPJ;
    property nApol: string        read FnApol   write FnApol;
    property aver: TAverCollection read FAver   write FAver;
  end;

  TAverCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TAverCollectionItem;
    procedure SetItem(Index: Integer; Value: TAverCollectionItem);
  public
    function Add: TAverCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TAverCollectionItem;
    property Items[Index: Integer]: TAverCollectionItem read GetItem write SetItem; default;
  end;

  TAverCollectionItem = class(TObject)
  private
    FnAver: string;
  public
    property nAver: string        read FnAver   write FnAver;
  end;

  Ttot = class(TObject)
  private
    FqCTe: Integer;
    FqCT: Integer;
    FqNFe: Integer;
    FqNF: Integer;
    FqMDFe: Integer;
    FvCarga: Double;
    FcUnid: TUnidMed;
    FqCarga: Double;
  public
    property qCTe: Integer   read FqCTe   write FqCTe;
    property qCT: Integer    read FqCT    write FqCT;
    property qNFe: Integer   read FqNFe   write FqNFe;
    property qNF: Integer    read FqNF    write FqNF;
    property qMDFe: Integer  read FqMDFe  write FqMDFe;
    property vCarga: Double  read FvCarga write FvCarga;
    property cUnid: TUnidMed read FcUnid  write FcUnid;
    property qCarga: Double  read FqCarga write FqCarga;
  end;

  TautXMLCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    function Add: TautXMLCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  TautXMLCollectionItem = class(TObject)
  private
    FCNPJCPF: string;
  public
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
  end;

  TinfAdic = class(TObject)
  private
    FinfAdFisco: string;
    FinfCpl: string;
  public
    property infAdFisco: string read FinfAdFisco write FinfAdFisco;
    property infCpl: string     read FinfCpl     write FinfCpl;
  end;

  TinfRespTec = class(TObject)
  private
    FCNPJ: string;
    FxContato: string;
    Femail: string;
    Ffone: string;
    FidCSRT: Integer;
    FhashCSRT: string;
  public
    property CNPJ: string     read FCNPJ     write FCNPJ;
    property xContato: string read FxContato write FxContato;
    property email: string    read Femail    write Femail;
    property fone: string     read Ffone     write Ffone;
    property idCSRT: Integer  read FidCSRT   write FidCSRT;
    property hashCSRT: string read FhashCSRT write FhashCSRT;
 end;

  TinfMDFeSupl = class(TObject)
  private
    FqrCodMDFe: string;
  public
    property qrCodMDFe: string read FqrCodMDFe write FqrCodMDFe;
  end;

  TinfLocal = class(TObject)
  private
    FCEP: Integer;
    Flatitude: Double;
    Flongitude: Double;
  public
    property CEP: Integer      read FCEP       write FCEP;
    property latitude: Double  read Flatitude  write Flatitude;
    property longitude: Double read Flongitude write Flongitude;
  end;

  TprodPred = class(TObject)
  private
    FtpCarga: TCarga;
    FxProd: string;
    FcEAN: string;
    FNCM: string;
    FinfLocalCarrega: TinfLocal;
    FinfLocalDescarrega: TinfLocal;
  public
    constructor Create;
    destructor Destroy; override;

    property tpCarga: TCarga read FtpCarga write FtpCarga;
    property xProd: string   read FxProd   write FxProd;
    property cEAN: string    read FcEAN    write FcEAN;
    property NCM: string     read FNCM     write FNCM;

    property infLocalCarrega: TinfLocal    read FinfLocalCarrega    write FinfLocalCarrega;
    property infLocalDescarrega: TinfLocal read FinfLocalDescarrega write FinfLocalDescarrega;
  end;

  TMDFe = class(TObject)
  private
    FinfMDFe: TinfMDFe;
    FIde: TIde;
    Femit: Temit;

    Frodo: Trodo;
    Faereo: Taereo;
    Faquav: Taquav;
    Fferrov: Tferrov;

    FinfDoc: TinfDoc;
    Fseg: TSegCollection;
    Ftot: Ttot;
    Flacres: TlacresCollection;
    FautXML: TautXMLCollection;
    FinfAdic: TinfAdic;
    FinfRespTec: TinfRespTec;
    FinfMDFeSupl: TinfMDFeSupl;
    FprodPred: TprodPred;

    FProcMDFe: TProcMDFe;
    FSignature: TSignature;

    procedure Setlacres(const Value: TlacresCollection);
    procedure SetautXML(const Value: TautXMLCollection);
    procedure SetSeg(const Value: TSegCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property infMDFe: TinfMDFe  read FinfMDFe write FinfMDFe;
    property Ide: TIde          read FIde     write FIde;
    property emit: Temit        read Femit    write Femit;

    property rodo: Trodo     read Frodo   write Frodo;
    property aereo: Taereo   read Faereo  write Faereo;
    property aquav: Taquav   read Faquav  write Faquav;
    property ferrov: Tferrov read Fferrov write Fferrov;

    property infDoc: TinfDoc           read FinfDoc  write FinfDoc;
    property seg: TSegCollection       read Fseg     write SetSeg;
    property tot: Ttot                 read Ftot     write Ftot;
    property lacres: TlacresCollection read Flacres  write Setlacres;
    property autXML: TautXMLCollection read FautXML  write SetautXML;
    property infAdic: TinfAdic         read FinfAdic write FinfAdic;

    property infRespTec: TinfRespTec   read FinfRespTec  write FinfRespTec;
    property infMDFeSupl: TinfMDFeSupl read FinfMDFeSupl write FinfMDFeSupl;
    property prodPred: TprodPred        read FprodPred    write FprodPred;

    property procMDFe: TProcMDFe   read FProcMDFe  write FProcMDFe;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

const
  CMUN_EXTERIOR = 9999999;
  XMUN_EXTERIOR = 'EXTERIOR';
  UF_EXTERIOR = 'EX';

implementation

uses
  ACBrUtil.Base;

{ TMDFe }

constructor TMDFe.Create;
begin
  inherited Create;
  FinfMDFe := TInfMDFe.Create;
  Fide     := Tide.Create;
  Femit    := Temit.Create;

  Frodo   := Trodo.Create;
  Faereo  := Taereo.Create;
  Faquav  := Taquav.Create;
  Fferrov := Tferrov.Create;

  FinfDoc  := TinfDoc.Create;
  Fseg     := TSegCollection.Create;
  Ftot     := Ttot.Create;
  Flacres  := TlacresCollection.Create;
  FautXML  := TautXMLCollection.Create;
  FinfAdic := TinfAdic.Create;

  FinfRespTec  := TinfRespTec.Create;
  FinfMDFeSupl := TinfMDFeSupl.Create;
  FprodPred    := TprodPred.Create;

  FProcMDFe  := TProcMDFe.create;
  Fsignature := Tsignature.create;
end;

destructor TMDFe.Destroy;
begin
  FinfMDFe.Free;
  Fide.Free;
  Femit.Free;

  Frodo.Free;
  Faereo.Free;
  Faquav.Free;
  Fferrov.Free;

  FinfDoc.Free;
  Fseg.Free;
  Ftot.Free;
  Flacres.Free;
  FautXML.Free;
  FinfAdic.Free;
  FinfRespTec.Free;
  FinfMDFeSupl.Free;
  FprodPred.Free;

  FProcMDFe.Free;
  Fsignature.Free;

  inherited;
end;

procedure TMDFe.Setlacres(const Value: TlacresCollection);
begin
  Flacres.Assign(Value);
end;

procedure TMDFe.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

procedure TMDFe.SetSeg(const Value: TSegCollection);
begin
  Fseg := Value;
end;

{ TinfMunCarregaCollection }

function TinfMunCarregaCollection.Add: TinfMunCarregaCollectionItem;
begin
  Result := Self.New;
end;

function TinfMunCarregaCollection.GetItem(Index: Integer): TinfMunCarregaCollectionItem;
begin
  Result := TinfMunCarregaCollectionItem(inherited Items[Index]);
end;

procedure TinfMunCarregaCollection.SetItem(Index: Integer; Value: TinfMunCarregaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfMunCarregaCollection.New: TinfMunCarregaCollectionItem;
begin
  Result := TinfMunCarregaCollectionItem.Create;
  Self.Add(Result);
end;

{ TIde }

constructor TIde.Create;
begin
  inherited Create;
  FinfMunCarrega := TinfMunCarregaCollection.Create;
  FinfPercurso   := TinfPercursoCollection.Create;
  FindCanalVerde := tiNao;
  FindCarregaPosterior := tiNao;
end;

destructor TIde.Destroy;
begin
  FinfMunCarrega.Free;
  FinfPercurso.Free;
  inherited;
end;

procedure TIde.SetinfMunCarrega(Value: TinfMunCarregaCollection);
begin
  FinfMunCarrega.Assign(Value);
end;

procedure TIde.SetinfPercurso(Value: TinfPercursoCollection);
begin
  FinfPercurso.Assign(Value);
end;

{ TinfPercursoCollection }

function TinfPercursoCollection.Add: TinfPercursoCollectionItem;
begin
  Result := Self.New;
end;

function TinfPercursoCollection.GetItem(
  Index: Integer): TinfPercursoCollectionItem;
begin
  Result := TinfPercursoCollectionItem(inherited Items[Index]);
end;

procedure TinfPercursoCollection.SetItem(Index: Integer;
  Value: TinfPercursoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfPercursoCollection.New: TinfPercursoCollectionItem;
begin
  Result := TinfPercursoCollectionItem.Create;
  Self.Add(Result);
end;

{ Temit }

constructor Temit.Create;
begin
  inherited Create;
  FenderEmit := TenderEmit.Create;
end;

destructor Temit.Destroy;
begin
  FenderEmit.Free;
  inherited;
end;

{ TRodo }

constructor TRodo.Create;
begin
  inherited Create;
  FinfANTT     := TinfANTT.Create;
  FveicTracao  := TveicTracao.Create;
  FveicReboque := TveicReboqueCollection.Create;
  FvalePed     := TvalePed.Create;
  FlacRodo     := TlacRodoCollection.Create;
end;

destructor TRodo.Destroy;
begin
  FinfANTT.Free;
  FveicTracao.Free;
  FveicReboque.Free;
  FvalePed.Free;
  FlacRodo.Free;
  inherited;
end;

procedure TRodo.SetveicReboque(const Value: TveicReboqueCollection);
begin
  FveicReboque.Assign(Value);
end;

{ TveicTracao }

constructor TveicTracao.Create;
begin
  inherited Create;
  Fprop     := Tprop.Create;
  Fcondutor := TcondutorCollection.Create;
end;

destructor TveicTracao.Destroy;
begin
  Fprop.Free;
  Fcondutor.Free;
  inherited;
end;

procedure TveicTracao.Setcondutor(const Value: TcondutorCollection);
begin
  Fcondutor.Assign(Value);
end;

{ TcondutorCollection }

function TcondutorCollection.Add: TcondutorCollectionItem;
begin
  Result := Self.New;
end;

function TcondutorCollection.GetItem(
  Index: Integer): TcondutorCollectionItem;
begin
  Result := TcondutorCollectionItem(inherited Items[Index]);
end;

procedure TcondutorCollection.SetItem(Index: Integer;
  Value: TcondutorCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TcondutorCollection.New: TcondutorCollectionItem;
begin
  Result := TcondutorCollectionItem.Create;
  Self.Add(Result);
end;

{ TveicReboqueCollection }

function TveicReboqueCollection.Add: TveicReboqueCollectionItem;
begin
  Result := Self.New;
end;

function TveicReboqueCollection.GetItem(
  Index: Integer): TveicReboqueCollectionItem;
begin
  Result := TveicReboqueCollectionItem(inherited Items[Index]);
end;

procedure TveicReboqueCollection.SetItem(Index: Integer;
  Value: TveicReboqueCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TveicReboqueCollection.New: TveicReboqueCollectionItem;
begin
  Result := TveicReboqueCollectionItem.Create;
  Self.Add(Result);
end;

{ TveicReboqueCollectionItem }

constructor TveicReboqueCollectionItem.Create;
begin
  inherited Create;
  Fprop := Tprop.Create;
end;

destructor TveicReboqueCollectionItem.Destroy;
begin
  Fprop.Free;
  inherited;
end;

{ TvalePed }

constructor TvalePed.Create;
begin
  inherited Create;
  Fdisp := TdispCollection.Create;
end;

destructor TvalePed.Destroy;
begin
  Fdisp.Free;
  inherited;
end;

procedure TvalePed.Setdisp(const Value: TdispCollection);
begin
  Fdisp.Assign(Value);
end;

{ TdispCollection }

function TdispCollection.Add: TdispCollectionItem;
begin
  Result := Self.New;
end;

function TdispCollection.GetItem(Index: Integer): TdispCollectionItem;
begin
  Result := TdispCollectionItem(inherited Items[Index]);
end;

procedure TdispCollection.SetItem(Index: Integer;
  Value: TdispCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TdispCollection.New: TdispCollectionItem;
begin
  Result := TdispCollectionItem.Create;
  Self.Add(Result);
end;

{ Tferrov }

constructor Tferrov.Create;
begin
  inherited Create;
  Fvag := TvagCollection.Create;
end;

destructor Tferrov.Destroy;
begin
  Fvag.Free;
  inherited;
end;

procedure Tferrov.Setvag(const Value: TvagCollection);
begin
  Fvag.Assign(Value);
end;

{ TvagCollection }

function TvagCollection.Add: TvagCollectionItem;
begin
  Result := Self.New;
end;

function TvagCollection.GetItem(Index: Integer): TvagCollectionItem;
begin
  Result := TvagCollectionItem(inherited Items[Index]);
end;

procedure TvagCollection.SetItem(Index: Integer;
  Value: TvagCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TvagCollection.New: TvagCollectionItem;
begin
  Result := TvagCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfDoc }

constructor TinfDoc.Create;
begin
  inherited Create;
  FinfMunDescarga := TinfMunDescargaCollection.Create;
end;

destructor TinfDoc.Destroy;
begin
  FinfMunDescarga.Free;
  inherited;
end;

procedure TinfDoc.SetinfMunDescarga(
  const Value: TinfMunDescargaCollection);
begin
  FinfMunDescarga.Assign(Value);
end;

{ TinfMunDescargaCollection }

function TinfMunDescargaCollection.Add: TinfMunDescargaCollectionItem;
begin
  Result := Self.New;
end;

function TinfMunDescargaCollection.GetItem(
  Index: Integer): TinfMunDescargaCollectionItem;
begin
  Result := TinfMunDescargaCollectionItem(inherited Items[Index]);
end;

procedure TinfMunDescargaCollection.SetItem(Index: Integer;
  Value: TinfMunDescargaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfMunDescargaCollection.New: TinfMunDescargaCollectionItem;
begin
  Result := TinfMunDescargaCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfMunDescargaCollectionItem }

constructor TinfMunDescargaCollectionItem.Create;
begin
  inherited Create;
  FinfCTe        := TinfCTeCollection.Create;
  FinfCT         := TinfCTCollection.Create;
  FinfNFe        := TinfNFeCollection.Create;
  FinfNF         := TinfNFCollection.Create;
  FinfMDFeTransp := TinfMDFeTranspCollection.Create;
end;

destructor TinfMunDescargaCollectionItem.Destroy;
begin
  FinfCTe.Free;
  FinfCT.Free;
  FinfNFe.Free;
  FinfNF.Free;
  FinfMDFeTransp.Free;
  inherited;
end;

procedure TinfMunDescargaCollectionItem.SetinfCTe(
  const Value: TinfCTeCollection);
begin
  FinfCTe.Assign(Value);
end;

procedure TinfMunDescargaCollectionItem.SetinfCT(
  const Value: TinfCTCollection);
begin
  FinfCT.Assign(Value);
end;

procedure TinfMunDescargaCollectionItem.SetinfNFe(
  const Value: TinfNFeCollection);
begin
  FinfNFe.Assign(Value);
end;

procedure TinfMunDescargaCollectionItem.SetinfNF(
  const Value: TinfNFCollection);
begin
  FinfNF.Assign(Value);
end;

{ TinfCTeCollection }

function TinfCTeCollection.Add: TinfCTeCollectionItem;
begin
  Result := Self.New;
end;

function TinfCTeCollection.GetItem(Index: Integer): TinfCTeCollectionItem;
begin
  Result := TinfCTeCollectionItem(inherited Items[Index]);
end;

procedure TinfCTeCollection.SetItem(Index: Integer;
  Value: TinfCTeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfCTeCollection.New: TinfCTeCollectionItem;
begin
  Result := TinfCTeCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfCTeCollectionItem }

constructor TinfCTeCollectionItem.Create;
begin
  inherited Create;
  FinfUnidTransp     := TInfUnidTranspCTeCollection.Create;
  FPeri              := TPeriCTeCollection.Create;
  FinfEntregaParcial := TinfEntregaParcial.Create;
  FinfNFePrestParcial := TinfNFePrestParcialCollection.Create;

  FindPrestacaoParcial := tieNenhum;
end;

destructor TinfCTeCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FPeri.Free;
  FinfEntregaParcial.Free;
  FinfNFePrestParcial.Free;

  inherited;
end;

{ TinfCTCollection }

function TinfCTCollection.Add: TinfCTCollectionItem;
begin
  Result := Self.New;
end;

function TinfCTCollection.GetItem(Index: Integer): TinfCTCollectionItem;
begin
  Result := TinfCTCollectionItem(inherited Items[Index]);
end;

procedure TinfCTCollection.SetItem(Index: Integer;
  Value: TinfCTCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfCTCollection.New: TinfCTCollectionItem;
begin
  Result := TinfCTCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfCTCollectionItem }

constructor TinfCTCollectionItem.Create;
begin
  inherited Create;
  FinfUnidTransp := TInfUnidTranspCTCollection.Create;
end;

destructor TinfCTCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  inherited;
end;

{ TinfNFeCollection }

function TinfNFeCollection.Add: TinfNFeCollectionItem;
begin
  Result := Self.New;
end;

function TinfNFeCollection.GetItem(Index: Integer): TinfNFeCollectionItem;
begin
  Result := TinfNFeCollectionItem(inherited Items[Index]);
end;

procedure TinfNFeCollection.SetItem(Index: Integer;
  Value: TinfNFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfNFeCollection.New: TinfNFeCollectionItem;
begin
  Result := TinfNFeCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfNFeCollectionItem }

constructor TinfNFeCollectionItem.Create;
begin
  inherited Create;
  FinfUnidTransp := TInfUnidTranspNFeCollection.Create;
  FPeri := TPeriNFeCollection.Create;
end;

destructor TinfNFeCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FPeri.Free;
  inherited;
end;

procedure TinfNFeCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspNFeCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TinfNFeCollectionItem.SetPeri(const Value: TPeriNFeCollection);
begin
  Fperi := Value;
end;

{ TinfNFCollection }

function TinfNFCollection.Add: TinfNFCollectionItem;
begin
  Result := Self.New;
end;

function TinfNFCollection.GetItem(Index: Integer): TinfNFCollectionItem;
begin
  Result := TinfNFCollectionItem(inherited Items[Index]);
end;

procedure TinfNFCollection.SetItem(Index: Integer;
  Value: TinfNFCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfNFCollection.New: TinfNFCollectionItem;
begin
  Result := TinfNFCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfNFCollectionItem }

constructor TinfNFCollectionItem.Create;
begin
  inherited Create;
  FinfUnidTransp := TInfUnidTranspNFCollection.Create;
end;

destructor TinfNFCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  inherited;
end;

{ TlacresCollection }

function TlacresCollection.Add: TlacresCollectionItem;
begin
  Result := Self.New;
end;

function TlacresCollection.GetItem(Index: Integer): TlacresCollectionItem;
begin
  Result := TlacresCollectionItem(inherited Items[Index]);
end;

procedure TlacresCollection.SetItem(Index: Integer;
  Value: TlacresCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TlacresCollection.New: TlacresCollectionItem;
begin
  Result := TlacresCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfTermCarregCollection }

function TinfTermCarregCollection.Add: TinfTermCarregCollectionItem;
begin
  Result := Self.New;
end;

function TinfTermCarregCollection.GetItem(
  Index: Integer): TinfTermCarregCollectionItem;
begin
  Result := TinfTermCarregCollectionItem(inherited Items[Index]);
end;

procedure TinfTermCarregCollection.SetItem(Index: Integer;
  Value: TinfTermCarregCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfTermCarregCollection.New: TinfTermCarregCollectionItem;
begin
  Result := TinfTermCarregCollectionItem.Create;
  Self.Add(Result);
end;

{ Taquav }

constructor Taquav.Create;
begin
  inherited Create;
  FinfTermCarreg      := TinfTermCarregCollection.Create;
  FinfTermDescarreg   := TinfTermDescarregCollection.Create;
  FinfEmbComb         := TinfEmbCombCollection.Create;
  FinfUnidCargaVazia  := TinfUnidCargaVaziaCollection.Create;
  FinfUnidTranspVazia := TinfUnidTranspVaziaCollection.Create;
end;

destructor Taquav.Destroy;
begin
  FinfTermCarreg.Free;
  FinfTermDescarreg.Free;
  FinfEmbComb.Free;
  FinfUnidCargaVazia.Free;
  FinfUnidTranspVazia.Free;
  inherited;
end;

{ TinfTermDescarregCollection }

function TinfTermDescarregCollection.Add: TinfTermDescarregCollectionItem;
begin
  Result := Self.New;
end;

function TinfTermDescarregCollection.GetItem(
  Index: Integer): TinfTermDescarregCollectionItem;
begin
  Result := TinfTermDescarregCollectionItem(inherited Items[Index]);
end;

procedure TinfTermDescarregCollection.SetItem(Index: Integer;
  Value: TinfTermDescarregCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfTermDescarregCollection.New: TinfTermDescarregCollectionItem;
begin
  Result := TinfTermDescarregCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfEmbCombCollection }

function TinfEmbCombCollection.Add: TinfEmbCombCollectionItem;
begin
  Result := Self.New;
end;

function TinfEmbCombCollection.GetItem(
  Index: Integer): TinfEmbCombCollectionItem;
begin
  Result := TinfEmbCombCollectionItem(inherited Items[Index]);
end;

procedure TinfEmbCombCollection.SetItem(Index: Integer;
  Value: TinfEmbCombCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfEmbCombCollection.New: TinfEmbCombCollectionItem;
begin
  Result := TinfEmbCombCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfUnidCargaVaziaCollection }

function TinfUnidCargaVaziaCollection.Add: TinfUnidCargaVaziaCollectionItem;
begin
  Result := Self.New;
end;

function TinfUnidCargaVaziaCollection.GetItem(
  Index: Integer): TinfUnidCargaVaziaCollectionItem;
begin
  Result := TinfUnidCargaVaziaCollectionItem(inherited Items[Index]);
end;

procedure TinfUnidCargaVaziaCollection.SetItem(Index: Integer;
  Value: TinfUnidCargaVaziaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfUnidCargaVaziaCollection.New: TinfUnidCargaVaziaCollectionItem;
begin
  Result := TinfUnidCargaVaziaCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfUnidTranspVaziaCollection }

function TinfUnidTranspVaziaCollection.Add: TinfUnidTranspVaziaCollectionItem;
begin
  Result := Self.New;
end;

function TinfUnidTranspVaziaCollection.GetItem(
  Index: Integer): TinfUnidTranspVaziaCollectionItem;
begin
  Result := TinfUnidTranspVaziaCollectionItem(inherited Items[Index]);
end;

procedure TinfUnidTranspVaziaCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspVaziaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfUnidTranspVaziaCollection.New: TinfUnidTranspVaziaCollectionItem;
begin
  Result := TinfUnidTranspVaziaCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfUnidTranspCTeCollection }

function TinfUnidTranspCTeCollection.Add: TinfUnidTranspCollectionItem;
begin
  Result := Self.New;
end;

function TinfUnidTranspCTeCollection.GetItem(
  Index: Integer): TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem(inherited Items[Index]);
end;

procedure TinfUnidTranspCTeCollection.SetItem(Index: Integer;
  Value: TinfUnidTranspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfUnidTranspCTeCollection.New: TinfUnidTranspCollectionItem;
begin
  Result := TinfUnidTranspCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfUnidTranspCollectionItem }

constructor TinfUnidTranspCollectionItem.Create;
begin
  inherited Create;
  FlacUnidTransp := TlacUnidTranspCollection.Create;
  FinfUnidCarga  := TinfUnidCargaCollection.Create;
end;

destructor TinfUnidTranspCollectionItem.Destroy;
begin
  FlacUnidTransp.Free;
  FinfUnidCarga.Free;
  inherited;
end;

{ TinfUnidCargaCollection }

function TinfUnidCargaCollection.Add: TinfUnidCargaCollectionItem;
begin
  Result := Self.New;
end;

function TinfUnidCargaCollection.GetItem(
  Index: Integer): TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem(inherited Items[Index]);
end;

procedure TinfUnidCargaCollection.SetItem(Index: Integer;
  Value: TinfUnidCargaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfUnidCargaCollection.New: TinfUnidCargaCollectionItem;
begin
  Result := TinfUnidCargaCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfUnidCargaCollectionItem }

constructor TinfUnidCargaCollectionItem.Create;
begin
  inherited Create;
  FlacUnidCarga := TlacUnidCargaCollection.Create;
end;

destructor TinfUnidCargaCollectionItem.Destroy;
begin
  FlacUnidCarga.Free;
  inherited;
end;

{ TinfMDFeTranspCollection }

function TinfMDFeTranspCollection.Add: TinfMDFeTranspCollectionItem;
begin
  Result := Self.New;
end;

function TinfMDFeTranspCollection.GetItem(
  Index: Integer): TinfMDFeTranspCollectionItem;
begin
  Result := TinfMDFeTranspCollectionItem(inherited Items[Index]);
end;

procedure TinfMDFeTranspCollection.SetItem(Index: Integer;
  Value: TinfMDFeTranspCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfMDFeTranspCollection.New: TinfMDFeTranspCollectionItem;
begin
  Result := TinfMDFeTranspCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfMDFeTranspCollectionItem }

constructor TinfMDFeTranspCollectionItem.Create;
begin
  inherited Create;
  FinfUnidTransp := TInfUnidTranspMDFeCollection.Create;
  FPeri := TPeriMDFeCollection.Create;
end;

destructor TinfMDFeTranspCollectionItem.Destroy;
begin
  FinfUnidTransp.Free;
  FPeri.Free;
  inherited;
end;

procedure TinfMDFeTranspCollectionItem.SetinfUnidTransp(
  const Value: TinfUnidTranspMDFeCollection);
begin
  FinfUnidTransp := Value;
end;

procedure TinfMDFeTranspCollectionItem.SetPeri(
  const Value: TPeriMDFeCollection);
begin
  Fperi := Value;
end;

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := Self.New;
end;

function TautXMLCollection.GetItem(Index: Integer): TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Items[Index]);
end;

procedure TautXMLCollection.SetItem(Index: Integer;
  Value: TautXMLCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TautXMLCollection.New: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfMDFe }
{
function TinfMDFe.GetVersaoStr: string;
begin
  if FVersao <= 0 then
     Result := V1_00
  else
     Result := 'versao="'+FloatToString(FVersao,'.','#0.00')+'"';
end;
}
{ TPeriCTeCollection }

function TPeriCTeCollection.Add: TPeriCollectionItem;
begin
  Result := Self.New;
end;

function TPeriCTeCollection.GetItem(Index: Integer): TPeriCollectionItem;
begin
  Result := TPeriCollectionItem(inherited Items[Index]);
end;

procedure TPeriCTeCollection.SetItem(Index: Integer;
  Value: TPeriCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TPeriCTeCollection.New: TPeriCollectionItem;
begin
  Result := TPeriCollectionItem.Create;
  Self.Add(Result);
end;

{ TSegCollectionItem }

constructor TSegCollectionItem.Create;
begin
  inherited Create;
  FAver := TAverCollection.Create;
end;

destructor TSegCollectionItem.Destroy;
begin
  FAver.Free;
  inherited;
end;

{ TSegCollection }

function TSegCollection.Add: TSegCollectionItem;
begin
  Result := Self.New;
end;

function TSegCollection.GetItem(Index: Integer): TSegCollectionItem;
begin
  Result := TSegCollectionItem(inherited Items[Index]);
end;

procedure TSegCollection.SetItem(Index: Integer;
  Value: TSegCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TSegCollection.New: TSegCollectionItem;
begin
  Result := TSegCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfANTT }

constructor TinfANTT.Create;
begin
  inherited Create;

  FinfCIOT        := TinfCIOTCollection.Create;
  FinfContratante := TinfContratanteCollection.Create;
  FvalePed        := TvalePed.Create;
  FinfPag         := TinfPagCollection.Create;
end;

destructor TinfANTT.Destroy;
begin
  FinfCIOT.Free;
  FinfContratante.Free;
  FvalePed.Free;
  FinfPag.Free;

  inherited;
end;

{ TinfCIOTCollection }

function TinfCIOTCollection.Add: TinfCIOTCollectionItem;
begin
  Result := Self.New;
end;

function TinfCIOTCollection.GetItem(
  Index: Integer): TinfCIOTCollectionItem;
begin
  Result := TinfCIOTCollectionItem(inherited Items[Index]);
end;

procedure TinfCIOTCollection.SetItem(Index: Integer;
  Value: TinfCIOTCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfCIOTCollection.New: TinfCIOTCollectionItem;
begin
  Result := TinfCIOTCollectionItem.Create;
  Self.Add(Result);
end;

{ TinfContratanteCollection }

function TinfContratanteCollection.Add: TinfContratanteCollectionItem;
begin
  Result := Self.New;
end;

function TinfContratanteCollection.GetItem(
  Index: Integer): TinfContratanteCollectionItem;
begin
  Result := TinfContratanteCollectionItem(inherited Items[Index]);
end;

procedure TinfContratanteCollection.SetItem(Index: Integer;
  Value: TinfContratanteCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TinfContratanteCollection.New: TinfContratanteCollectionItem;
begin
  Result := TinfContratanteCollectionItem.Create;
  Self.Add(Result);
end;

{ TAverCollection }

function TAverCollection.Add: TAverCollectionItem;
begin
  Result := Self.New;
end;

function TAverCollection.GetItem(Index: Integer): TAverCollectionItem;
begin
  Result := TAverCollectionItem(inherited Items[Index]);
end;

procedure TAverCollection.SetItem(Index: Integer; Value: TAverCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TAverCollection.New: TAverCollectionItem;
begin
  Result := TAverCollectionItem.Create;
  Self.Add(Result);
end;

{ TprodPred }

constructor TprodPred.Create;
begin
  inherited Create;

  FinfLocalCarrega    := TinfLocal.Create;
  FinfLocalDescarrega := TinfLocal.Create;
end;

destructor TprodPred.Destroy;
begin
  FinfLocalCarrega.Free;
  FinfLocalDescarrega.Free;

  inherited;
end;

{ TinfPag }

constructor TinfPagCollectionItem.Create;
begin
  inherited Create;

  FComp     := TCompCollection.Create;
  FinfPrazo := TInfPrazoCollection.Create;
  FinfBanc  := TinfBanc.Create;
  FindAntecipaAdiant := tiNao;
end;

destructor TinfPagCollectionItem.Destroy;
begin
  FComp.Free;
  FinfPrazo.Free;
  FinfBanc.Free;

  inherited;
end;

{ TCompCollection }

function TCompCollection.Add: TCompCollectionItem;
begin
  Result := Self.New;
end;

function TCompCollection.GetItem(Index: Integer): TCompCollectionItem;
begin
  Result := TCompCollectionItem(inherited Items[Index]);
end;

function TCompCollection.New: TCompCollectionItem;
begin
  Result := TCompCollectionItem.Create;
  Self.Add(Result);
end;

procedure TCompCollection.SetItem(Index: Integer;
  Value: TCompCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TInfPrazoCollection }

function TInfPrazoCollection.Add: TInfPrazoCollectionItem;
begin
  Result := Self.New;
end;

function TInfPrazoCollection.GetItem(
  Index: Integer): TInfPrazoCollectionItem;
begin
  Result := TInfPrazoCollectionItem(inherited Items[Index]);
end;

function TInfPrazoCollection.New: TInfPrazoCollectionItem;
begin
  Result := TInfPrazoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TInfPrazoCollection.SetItem(Index: Integer;
  Value: TInfPrazoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfPagCollection }

function TinfPagCollection.Add: TinfPagCollectionItem;
begin
  Result := Self.New;
end;

function TinfPagCollection.GetItem(Index: Integer): TinfPagCollectionItem;
begin
  Result := TinfPagCollectionItem(inherited Items[Index]);
end;

function TinfPagCollection.New: TinfPagCollectionItem;
begin
  Result := TinfPagCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfPagCollection.SetItem(Index: Integer;
  Value: TinfPagCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfContratanteCollectionItem }

constructor TinfContratanteCollectionItem.Create;
begin
  inherited Create;

  FinfContrato := TinfContrato.Create;
end;

destructor TinfContratanteCollectionItem.Destroy;
begin
  FinfContrato.Free;

  inherited;
end;

{ TinfNFePrestParcialCollection }

function TinfNFePrestParcialCollection.Add: TinfNFePrestParcialCollectionItem;
begin
  Result := Self.New;
end;

function TinfNFePrestParcialCollection.GetItem(
  Index: Integer): TinfNFePrestParcialCollectionItem;
begin
  Result := TinfNFePrestParcialCollectionItem(inherited Items[Index]);
end;

function TinfNFePrestParcialCollection.New: TinfNFePrestParcialCollectionItem;
begin
  Result := TinfNFePrestParcialCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfNFePrestParcialCollection.SetItem(Index: Integer;
  Value: TinfNFePrestParcialCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.

