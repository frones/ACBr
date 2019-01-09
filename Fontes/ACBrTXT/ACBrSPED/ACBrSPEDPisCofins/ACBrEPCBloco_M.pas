{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Isaque Pinheiro                      }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 14/12/2010: Isaque Pinheiro e Paulo Junqueira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEPCBloco_M;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEPCBlocos;

type
  TRegistroM400     = class;
  TRegistroM410     = class;
  TRegistroM800     = class;
  TRegistroM810     = class;
  TRegistroM100List = class;
  TRegistroM105List = class;
  TRegistroM110List = class;
  TRegistroM115List = class;
  TRegistroM200     = class;
  TRegistroM205List = class;
  TRegistroM210List = class;
  TRegistroM211     = class;
  TRegistroM215     = class;
  TRegistroM220List = class;
  TRegistroM225List = class;
  TRegistroM230List = class;
  TRegistroM300List = class;
  TRegistroM350     = class;
  TRegistroM400List = class;
  TRegistroM410List = class;
  TRegistroM500List = class;
  TRegistroM505List = class;
  TRegistroM510List = class;
  TRegistroM515List = class;
  TRegistroM600     = class;
  TRegistroM605List = class;
  TRegistroM610List = class;
  TRegistroM611     = class;
  TRegistroM615     = class;
  TRegistroM620List = class;
  TRegistroM625List = class;
  TRegistroM630List = class;
  TRegistroM700List = class;
  TRegistroM800List = class;
  TRegistroM810List = class;

  //REGISTRO M001: ABERTURA DO BLOCO M
  TRegistroM001 = class(TOpenBlocos)
  private
    FRegistroM100         : TRegistroM100List; // NIVEL 2
    FRegistroM200         : TRegistroM200;     // NIVEL 2
    FRegistroM300         : TRegistroM300List; // NIVEL 2
    FRegistroM350         : TRegistroM350;     // NIVEL 2
    FRegistroM400         : TRegistroM400List; // NIVEL 2
    FRegistroM500         : TRegistroM500List; // NIVEL 2
    FRegistroM600         : TRegistroM600;     // NIVEL 2
    FRegistroM700         : TRegistroM700List; // NIVEL 2
    FRegistroM800         : TRegistroM800List; // NIVEL 2
  public
    constructor Create;  virtual;              /// Create
    destructor  Destroy; override;             /// Destroy
    function LocalizaRegistroM400(ACST_PIS: TACBrCstPis;
                                  const ACOD_CTA: String='';
                                  const ADESC_COMPL : String='') : TRegistroM400;
    function LocalizaRegistroM800(ACST_COFINS: TACBrCstCofins;
                                  const ACOD_CTA: String='';
                                  const ADESC_COMPL : String='') : TRegistroM800;
    property RegistroM100 : TRegistroM100List read FRegistroM100 write FRegistroM100;
    property RegistroM200 : TRegistroM200     read FRegistroM200 write FRegistroM200;
    property RegistroM300 : TRegistroM300List read FRegistroM300 write FRegistroM300;
    property RegistroM350 : TRegistroM350     read FRegistroM350 write FRegistroM350;
    property RegistroM400 : TRegistroM400List read FRegistroM400 write FRegistroM400;
    property RegistroM500 : TRegistroM500List read FRegistroM500 write FRegistroM500;
    property RegistroM600 : TRegistroM600     read FRegistroM600 write FRegistroM600;
    property RegistroM700 : TRegistroM700List read FRegistroM700 write FRegistroM700;
    property RegistroM800 : TRegistroM800List read FRegistroM800 write FRegistroM800;
  end;


  //REGISTRO M100: ABERTURA DO BLOCO M
  TRegistroM100 = class
  private
    FALIQ_PIS               : Variant;
    FVL_CRED_DESC           : Variant;
    FVL_AJUS_ACRES          : currency;
    FVL_CRED_DIF            : currency;
    FSLD_CRED               : currency;
    FVL_CRED                : currency;
    FALIQ_PIS_QUANT         : Variant;
    FVL_AJUS_REDUC          : currency;
    FVL_CRED_DISP           : currency;
    FQUANT_BC_PIS           : Variant;
    FVL_BC_PIS              : Variant;
    FIND_CRED_ORI           : TACBrIndCredOri;
    FCOD_CRED               : string;
    FIND_DESC_CRED          : TACBrIndDescCred;

    FRegistroM105           : TRegistroM105List; // NIVEL 3
    FRegistroM110           : TRegistroM110List; // NIVEL 3
  public
    constructor Create;  virtual;                /// Create
    destructor  Destroy; override;               /// Destroy

    property COD_CRED       : String            read FCOD_CRED       write FCOD_CRED;
    property IND_CRED_ORI   : TACBrIndCredOri   read FIND_CRED_ORI   write FIND_CRED_ORI;
    property VL_BC_PIS      : Variant           read FVL_BC_PIS      write FVL_BC_PIS;
    property ALIQ_PIS       : Variant           read FALIQ_PIS       write FALIQ_PIS;
    property QUANT_BC_PIS   : Variant           read FQUANT_BC_PIS   write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT : Variant           read FALIQ_PIS_QUANT write FALIQ_PIS_QUANT;
    property VL_CRED        : currency          read FVL_CRED        write FVL_CRED;
    property VL_AJUS_ACRES  : currency          read FVL_AJUS_ACRES  write FVL_AJUS_ACRES;
    property VL_AJUS_REDUC  : currency          read FVL_AJUS_REDUC  write FVL_AJUS_REDUC;
    property VL_CRED_DIF    : currency          read FVL_CRED_DIF    write FVL_CRED_DIF;
    property VL_CRED_DISP   : currency          read FVL_CRED_DISP   write FVL_CRED_DISP;
    property IND_DESC_CRED  : TACBrIndDescCred  read FIND_DESC_CRED  write FIND_DESC_CRED;
    property VL_CRED_DESC   : Variant           read FVL_CRED_DESC   write FVL_CRED_DESC;
    property SLD_CRED       : currency          read FSLD_CRED       write FSLD_CRED;

    property RegistroM105   : TRegistroM105List read FRegistroM105   write FRegistroM105;
    property RegistroM110   : TRegistroM110List read FRegistroM110   write FRegistroM110;
  end;

  // Registro M100 - Lista
  TRegistroM100List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM100;
    procedure SetItem(Index: Integer; const Value: TRegistroM100);
  public
    function New: TRegistroM100;
    property Items[Index: Integer]: TRegistroM100 read GetItem write SetItem;
  end;

  //REGISTRO M105: DETALHAMENTO DA BASE DE CALCULO DO CRÉDITO APURADO NO PERÍODO - PIS/PASEP
  TRegistroM105 = class
  private
    FQUANT_BC_PIS             : Variant;
    FVL_BC_PIS_CUM            : Variant;
    FQUANT_BC_PIS_TOT         : Variant;
    FVL_BC_PIS_TOT            : Variant;
    FVL_BC_PIS_NC             : Variant;
    FVL_BC_PIS                : Variant;
    FCST_PIS                  : TACBrCstPis;
    FDESC_CRED                : string;
    FNAT_BC_CRED              : TACBrBaseCalculoCredito;
  public
    property NAT_BC_CRED      : TACBrBaseCalculoCredito read FNAT_BC_CRED      write FNAT_BC_CRED;
    property CST_PIS          : TACBrCstPis             read FCST_PIS          write FCST_PIS;
    property VL_BC_PIS_TOT    : Variant                 read FVL_BC_PIS_TOT    write FVL_BC_PIS_TOT;
    property VL_BC_PIS_CUM    : Variant                 read FVL_BC_PIS_CUM    write FVL_BC_PIS_CUM;
    property VL_BC_PIS_NC     : Variant                 read FVL_BC_PIS_NC     write FVL_BC_PIS_NC;
    property VL_BC_PIS        : Variant                 read FVL_BC_PIS        write FVL_BC_PIS;
    property QUANT_BC_PIS_TOT : Variant                 read FQUANT_BC_PIS_TOT write FQUANT_BC_PIS_TOT;
    property QUANT_BC_PIS     : Variant                 read FQUANT_BC_PIS     write FQUANT_BC_PIS;
    property DESC_CRED        : string                  read FDESC_CRED        write FDESC_CRED;
  end;

  // Registro M105 - Lista
  TRegistroM105List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM105;
    procedure SetItem(Index: Integer; const Value: TRegistroM105);
  public
    function New: TRegistroM105;
    property Items[Index: Integer]: TRegistroM105 read GetItem write SetItem;
  end;

  //REGISTRO M110: AJUSTES DO CRÉDITO DE PIS/PASEP APURADO

  { TRegistroM110 }

  TRegistroM110 = class
  private
    FRegistroM115: TRegistroM115List;
    FVL_AJ            : currency;
    FNUM_DOC          : string;
    FCOD_AJ           : TACBrCodAj;
    FDESCR_AJ         : string;
    FIND_AJ           : TACBrIndAJ;
    FDT_REF           : TDateTime;
  public
    constructor Create;  virtual;                      /// Create
    destructor  Destroy; override;                     /// Destroy

    property IND_AJ   : TACBrIndAJ    read FIND_AJ   write FIND_AJ;
    property VL_AJ    : currency      read FVL_AJ    write FVL_AJ;
    property COD_AJ   : TACBrCodAj    read FCOD_AJ   write FCOD_AJ;
    property NUM_DOC  : string        read FNUM_DOC  write FNUM_DOC;
    property DESCR_AJ : string        read FDESCR_AJ write FDESCR_AJ;
    property DT_REF   : TDateTime     read FDT_REF   write FDT_REF;

    property RegistroM115   : TRegistroM115List read FRegistroM115   write FRegistroM115;
  end;

  // Registro M110 - Lista
  TRegistroM110List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM110;
    procedure SetItem(Index: Integer; const Value: TRegistroM110);
  public
    function New: TRegistroM110;
    property Items[Index: Integer]: TRegistroM110 read GetItem write SetItem;
  end;

  //REGISTRO M115: DETALHAMENTO DOS AJUSTES DO CRÉDITO DE PIS/PASEP APURADO
  TRegistroM115 = class
  private
    fCOD_CTA: string;
    fCST_PIS: TACBrCstPis;
    fDESC_AJ: string;
    fDET_ALIQ: Currency;
    fDET_BC_CRED: Currency;
    fDET_VALOR_AJ: Currency;
    fDT_OPER_AJ: TDateTime;
    fINFO_COMPL: string;
  public
    property DET_VALOR_AJ    : Currency read fDET_VALOR_AJ write fDET_VALOR_AJ;
    property CST_PIS         : TACBrCstPis read fCST_PIS write fCST_PIS;
    property DET_BC_CRED     : Currency read fDET_BC_CRED write fDET_BC_CRED;
    property DET_ALIQ        : Currency read fDET_ALIQ write fDET_ALIQ;
    property DT_OPER_AJ      : TDateTime read fDT_OPER_AJ write fDT_OPER_AJ;
    property DESC_AJ         : string read fDESC_AJ write fDESC_AJ;
    property COD_CTA         : string read fCOD_CTA write fCOD_CTA;
    property INFO_COMPL      : string read fINFO_COMPL write fINFO_COMPL;
  end;

  // Registro M115 - Lista
  TRegistroM115List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM115;
    procedure SetItem(Index: Integer; const Value: TRegistroM115);
  public
    function New: TRegistroM115;
    property Items[Index: Integer]: TRegistroM115 read GetItem write SetItem;
  end;

  //REGISTRO M200: CONSOLIDAÇÃO DA CONTRIBUIÇÃO PARA O PIS/PASEP DO PERÍODO
  TRegistroM200 = class
  private
    FVL_TOT_CONT_REC              : currency;
    FVL_TOT_CRED_DESC             : currency;
    FVL_OUT_DED_CUM               : currency;
    FVL_TOT_CONT_NC_DEV           : currency;
    FVL_RET_CUM                   : currency;
    FVL_RET_NC                    : currency;
    FVL_TOT_CONT_NC_PER           : currency;
    FVL_CONT_CUM_REC              : currency;
    FVL_CONT_NC_REC               : currency;
    FVL_OUT_DED_NC                : currency;
    FVL_TOT_CRED_DESC_ANT         : currency;
    FVL_TOT_CONT_CUM_PER          : currency;

    FRegistroM205                 : TRegistroM205List; // NIVEL 3
    FRegistroM210                 : TRegistroM210List; // NIVEL 3

  public
    constructor Create;  virtual;                      /// Create
    destructor  Destroy; override;                     /// Destroy

    property VL_TOT_CONT_NC_PER   : currency          read FVL_TOT_CONT_NC_PER   write FVL_TOT_CONT_NC_PER;
    property VL_TOT_CRED_DESC     : currency          read FVL_TOT_CRED_DESC     write FVL_TOT_CRED_DESC;
    property VL_TOT_CRED_DESC_ANT : currency          read FVL_TOT_CRED_DESC_ANT write FVL_TOT_CRED_DESC_ANT;
    property VL_TOT_CONT_NC_DEV   : currency          read FVL_TOT_CONT_NC_DEV   write FVL_TOT_CONT_NC_DEV;
    property VL_RET_NC            : currency          read FVL_RET_NC            write FVL_RET_NC;
    property VL_OUT_DED_NC        : currency          read FVL_OUT_DED_NC        write FVL_OUT_DED_NC;
    property VL_CONT_NC_REC       : currency          read FVL_CONT_NC_REC       write FVL_CONT_NC_REC;
    property VL_TOT_CONT_CUM_PER  : currency          read FVL_TOT_CONT_CUM_PER  write FVL_TOT_CONT_CUM_PER;
    property VL_RET_CUM           : currency          read FVL_RET_CUM           write FVL_RET_CUM;
    property VL_OUT_DED_CUM       : currency          read FVL_OUT_DED_CUM       write FVL_OUT_DED_CUM;
    property VL_CONT_CUM_REC      : currency          read FVL_CONT_CUM_REC      write FVL_CONT_CUM_REC;
    property VL_TOT_CONT_REC      : currency          read FVL_TOT_CONT_REC      write FVL_TOT_CONT_REC;

    property RegistroM205         : TRegistroM205List read FRegistroM205         write FRegistroM205;
    property RegistroM210         : TRegistroM210List read FRegistroM210         write FRegistroM210;
  end;

  //REGISTRO M205: CONTRIBUIÇÃO PARA O PIS/PASEP A RECOLHER – DETALHAMENTO POR CÓDIGO DE RECEITA
  TRegistroM205 = class
  private
    fCOD_REC: string;
    fNUM_CAMPO: string;
    fVL_DEBITO: currency;
  public
    property NUM_CAMPO: string read fNUM_CAMPO write fNUM_CAMPO;
    property COD_REC: string read fCOD_REC write fCOD_REC;
    property VL_DEBITO: currency read fVL_DEBITO write fVL_DEBITO;
  end;

  // Registro M205 - Lista
  TRegistroM205List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM205;
    procedure SetItem(Index: Integer; const Value: TRegistroM205);
  public
    function New: TRegistroM205;
    property Items[Index: Integer]: TRegistroM205 read GetItem write SetItem;
  end;

  //REGISTRO M210: DETALHAMENTO DA CONTRIBUIÇÃO PARA O PIS/PASEP DO PERÍODO
  TRegistroM210 = class
  private
    FALIQ_PIS                  : Variant;
    FVL_CONT_PER               : currency;
    FVL_CONT_DIFER             : Variant;
    FVL_AJUS_REDUC             : currency;
    FVL_AJUS_ACRES             : currency;
    FVL_CONT_DIFER_ANT         : Variant;
    FALIQ_PIS_QUANT            : Variant;
    FVL_BC_CONT                : currency;
    FVL_CONT_APUR              : currency;
    FVL_REC_BRT                : currency;
    FQUANT_BC_PIS              : Variant;
    FCOD_CONT                  : TACBrCodCont;
    FVL_AJUS_ACRES_BC_PIS      : currency;
    FVL_AJUS_REDUC_BC_PIS      : currency;
    FVL_BC_CONT_AJUS           : currency;

    FRegistroM211              : TRegistroM211;     // NIVEL 4
    FRegistroM215              : TRegistroM215;     // NIVEL 4
    FRegistroM220              : TRegistroM220List; // NIVEL 4
    FRegistroM230              : TRegistroM230List; // NIVEL 4
  public
    constructor Create; virtual;                    /// Create
    destructor Destroy; override;                   /// Destroy

    property COD_CONT             : TACBrCodCont      read FCOD_CONT             write FCOD_CONT;
    property VL_REC_BRT           : currency          read FVL_REC_BRT           write FVL_REC_BRT;
    property VL_BC_CONT           : currency          read FVL_BC_CONT           write FVL_BC_CONT;
    property VL_AJUS_ACRES_BC_PIS : currency          read FVL_AJUS_ACRES_BC_PIS write FVL_AJUS_ACRES_BC_PIS;
    property VL_AJUS_REDUC_BC_PIS : currency          read FVL_AJUS_REDUC_BC_PIS write FVL_AJUS_REDUC_BC_PIS;
    property VL_BC_CONT_AJUS      : currency          read FVL_BC_CONT_AJUS      write FVL_BC_CONT_AJUS;
    property ALIQ_PIS             : Variant           read FALIQ_PIS             write FALIQ_PIS;
    property QUANT_BC_PIS         : Variant           read FQUANT_BC_PIS         write FQUANT_BC_PIS;
    property ALIQ_PIS_QUANT       : Variant           read FALIQ_PIS_QUANT       write FALIQ_PIS_QUANT;
    property VL_CONT_APUR         : currency          read FVL_CONT_APUR         write FVL_CONT_APUR;
    property VL_AJUS_ACRES        : currency          read FVL_AJUS_ACRES        write FVL_AJUS_ACRES;
    property VL_AJUS_REDUC        : currency          read FVL_AJUS_REDUC        write FVL_AJUS_REDUC;
    property VL_CONT_DIFER        : Variant           read FVL_CONT_DIFER        write FVL_CONT_DIFER;
    property VL_CONT_DIFER_ANT    : Variant           read FVL_CONT_DIFER_ANT    write FVL_CONT_DIFER_ANT;
    property VL_CONT_PER          : currency          read FVL_CONT_PER          write FVL_CONT_PER;

    property RegistroM211         : TRegistroM211     read FRegistroM211         write FRegistroM211;
    property RegistroM215         : TRegistroM215     read FRegistroM215         write FRegistroM215;
    property RegistroM220         : TRegistroM220List read FRegistroM220         write FRegistroM220;
    property RegistroM230         : TRegistroM230List read FRegistroM230         write FRegistroM230;
  end;

  // Registro M210 - Lista
  TRegistroM210List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM210;
    procedure SetItem(Index: Integer; const Value: TRegistroM210);
  public
    function New: TRegistroM210;
    property Items[Index: Integer]: TRegistroM210 read GetItem write SetItem;
  end;

  //REGISTRO M211: SOCIEDADES COOPERATIVAS - COMPOSIÇÃO DA BASE DE CALCULO - PIS/PASEP
  TRegistroM211 = class
  private
    FVL_BC_CONT_ANT_EXC_COOP         : currency;
    FVL_EXC_COOP_GER                 : currency;
    FVL_BC_CONT                      : currency;
    FVL_EXC_ESP_COOP                 : currency;
    FIND_TIP_COOP                    : TACBrIndTipCoop;
  public
    property IND_TIP_COOP            : TACBrIndTipCoop   read FIND_TIP_COOP            write FIND_TIP_COOP;
    property VL_BC_CONT_ANT_EXC_COOP : currency          read FVL_BC_CONT_ANT_EXC_COOP write FVL_BC_CONT_ANT_EXC_COOP;
    property VL_EXC_COOP_GER         : currency          read FVL_EXC_COOP_GER         write FVL_EXC_COOP_GER;
    property VL_EXC_ESP_COOP         : currency          read FVL_EXC_ESP_COOP         write FVL_EXC_ESP_COOP;
    property VL_BC_CONT              : currency          read FVL_BC_CONT              write FVL_BC_CONT;
  end;


  //REGISTRO M215: Ajustes da Base de Cálculo da Contribuição para o PIS/Pasep Apurada
  { TRegistroM215 }
  TRegistroM215 = class
  private
    FVL_AJ_BC             : currency;
    FCNPJ                 : string;
    FDT_REF               : TDateTime;
    FDESCR_AJ_BC          : string;
    FIND_AJ_BC            : TACBrIndAJ;
    FNUM_DOC              : string;
    FINFO_COMPL           : string;
    FCOD_AJ_BC            : TACBrTabCodAjBaseCalcContrib;
    FCOD_CTA              : string;
  public
    property IND_AJ_BC    : TACBrIndAJ                    read FIND_AJ_BC       write FIND_AJ_BC;
    property VL_AJ_BC     : currency                      read FVL_AJ_BC        write FVL_AJ_BC;
    property COD_AJ_BC    : TACBrTabCodAjBaseCalcContrib  read FCOD_AJ_BC       write FCOD_AJ_BC;
    property NUM_DOC      : string                        read FNUM_DOC         write FNUM_DOC;
    property DESCR_AJ_BC  : string                        read FDESCR_AJ_BC     write FDESCR_AJ_BC;
    property DT_REF       : TDateTime                     read FDT_REF          write FDT_REF;
    property COD_CTA      : string                        read FCOD_CTA         write FCOD_CTA;
    property CNPJ         : string                        read FCNPJ            write FCNPJ;
    property INFO_COMPL   : string                        read FINFO_COMPL      write FINFO_COMPL;
  end;


  //REGISTRO M220: AJUSTES DA CONTRIBUIÇÃO PARA O PIS/PASEP APURADA

  { TRegistroM220 }

  TRegistroM220 = class
  private
    FRegistroM225: TRegistroM225List;
    FVL_AJ            : currency;
    FNUM_DOC          : string;
    FCOD_AJ           : TACBrCodAj;
    FDESCR_AJ         : string;
    FIND_AJ           : TACBrIndAJ;
    FDT_REF           : TDateTime;
  public
    constructor Create; virtual;                    /// Create
    destructor Destroy; override;                   /// Destroy

    property IND_AJ   : TACBrIndAJ    read FIND_AJ   write FIND_AJ;
    property VL_AJ    : currency      read FVL_AJ    write FVL_AJ;
    property COD_AJ   : TACBrCodAj    read FCOD_AJ   write FCOD_AJ;
    property NUM_DOC  : string        read FNUM_DOC  write FNUM_DOC;
    property DESCR_AJ : string        read FDESCR_AJ write FDESCR_AJ;
    property DT_REF   : TDateTime     read FDT_REF   write FDT_REF;

    property RegistroM225      : TRegistroM225List read FRegistroM225      write FRegistroM225;
  end;

  // Registro M220 - Lista
  TRegistroM220List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM220;
    procedure SetItem(Index: Integer; const Value: TRegistroM220);
  public
    function New: TRegistroM220;
    property Items[Index: Integer]: TRegistroM220 read GetItem write SetItem;
  end;

  //REGISTRO M225: DETALHAMENTO DOS AJUSTES DA CONTRIBUIÇÃO PARA O PIS/PASEP APURADA
  TRegistroM225 = class
  private
    FCOD_CTA: string;
    FCST_PIS: TACBrCstPis;
    FDESC_AJ: string;
    FDET_ALIQ: Currency;
    FDET_BC_CRED: Currency;
    FDET_VALOR_AJ: Currency;
    FDT_OPER_AJ: TDateTime;
    FINFO_COMPL: string;
  public
    property DET_VALOR_AJ : Currency read FDET_VALOR_AJ write FDET_VALOR_AJ;
    property CST_PIS      : TACBrCstPis read FCST_PIS write FCST_PIS;
    property DET_BC_CRED  : Currency read FDET_BC_CRED write FDET_BC_CRED;
    property DET_ALIQ     : Currency read FDET_ALIQ write FDET_ALIQ;
    property DT_OPER_AJ   : TDateTime read FDT_OPER_AJ write FDT_OPER_AJ;
    property DESC_AJ      : string read FDESC_AJ write FDESC_AJ;
    property COD_CTA      : string read FCOD_CTA write FCOD_CTA;
    property INFO_COMPL   : string read FINFO_COMPL write FINFO_COMPL;
  end;

  // Registro M225 - Lista
  TRegistroM225List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM225;
    procedure SetItem(Index: Integer; const Value: TRegistroM225);
  public
    function New: TRegistroM225;
    property Items[Index: Integer]: TRegistroM225 read GetItem write SetItem;
  end;

  //REGISTRO M230: INFORMAÇÕES ADICIONAIS DE DIFERIMENTO
  TRegistroM230 = class
  private
    FVL_CONT_DIF          : currency;
    FVL_NAO_RECEB         : currency;
    FVL_CRED_DIF          : currency;
    FVL_VEND              : currency;
    FCNPJ                 : string;
    FCOD_CRED             : string;
  public
    property CNPJ         : string       read FCNPJ         write FCNPJ;
    property VL_VEND      : currency     read FVL_VEND      write FVL_VEND;
    property VL_NAO_RECEB : currency     read FVL_NAO_RECEB write FVL_NAO_RECEB;
    property VL_CONT_DIF  : currency     read FVL_CONT_DIF  write FVL_CONT_DIF;
    property VL_CRED_DIF  : currency     read FVL_CRED_DIF  write FVL_CRED_DIF;
    property COD_CRED     : string       read FCOD_CRED     write FCOD_CRED;
  end;

  // Registro M230 - Lista
  TRegistroM230List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM230;
    procedure SetItem(Index: Integer; const Value: TRegistroM230);
  public
    function New: TRegistroM230;
    property Items[Index: Integer]: TRegistroM230 read GetItem write SetItem;
  end;

  //REGISTRO M300: CONTRIBUIÇÃO DE PIS/PASEP DIFERIDA EM PERÍODOS ANTERIORES - VALORES A PAGAR NO PERÍODO
  TRegistroM300 = class
  private
    FCOD_CONT                   : TACBrCodCont;           {COD_CONT}
    FVL_CONT_APUR_DIFER         : currency;               {VL_CONT_APUR_DIFER}
    FNAT_CRED_DESC              : TACBrNatCredDesc;       {NAT_CRED_DESC}
    FVL_CRED_DESC_DIFER         : currency;               {VL_CRED_DESC_DIFER}
    FVL_CONT_DIFER_ANT          : currency;               {VL_CONT_DIFER_ANT}
    FPER_APUR                   : string;                 {PER_APUR}
    FDT_RECEB                   : TDateTime;              {DT_RECEB}
  public
    property COD_CONT           : TACBrCodCont     read FCOD_CONT             write FCOD_CONT;
    property VL_CONT_APUR_DIFER : currency         read FVL_CONT_APUR_DIFER   write FVL_CONT_APUR_DIFER;
    property NAT_CRED_DESC      : TACBrNatCredDesc read FNAT_CRED_DESC        write FNAT_CRED_DESC;
    property VL_CRED_DESC_DIFER : currency         read FVL_CRED_DESC_DIFER   write FVL_CRED_DESC_DIFER;
    property VL_CONT_DIFER_ANT  : currency         read FVL_CONT_DIFER_ANT    write FVL_CONT_DIFER_ANT;
    property PER_APUR           : string           read FPER_APUR             write FPER_APUR;
    property DT_RECEB           : TDateTime        read FDT_RECEB             write FDT_RECEB;
  end;

  // Registro M300 - Lista
  TRegistroM300List = class(TObjectList)
  private
    function  GetItem(Index  : Integer): TRegistroM300;
    procedure SetItem(Index : Integer; const Value: TRegistroM300);
  public
    function New            : TRegistroM300;
    property Items[Index    : Integer]: TRegistroM300 read GetItem write SetItem;
  end;

  //REGISTRO M350: CONTRIBUIÇÃO DE PIS/PASEP DIFERIDA EM PERÍODOS ANTERIORES - VALORES A PAGAR NO PERÍODO
  TRegistroM350 = class
  private
    FVL_TOT_FOL              : currency;
    FVL_EXC_BC               : currency;
    FALIQ_PIS_FOL            : currency;
    FVL_TOT_BC               : currency;
    FVL_TOT_CONT_FOL         : currency;
  public
    property VL_TOT_FOL      : currency read FVL_TOT_FOL      write FVL_TOT_FOL;
    property VL_EXC_BC       : currency read FVL_EXC_BC       write FVL_EXC_BC;
    property VL_TOT_BC       : currency read FVL_TOT_BC       write FVL_TOT_BC;
    property ALIQ_PIS_FOL    : currency read FALIQ_PIS_FOL    write FALIQ_PIS_FOL;
    property VL_TOT_CONT_FOL : currency read FVL_TOT_CONT_FOL write FVL_TOT_CONT_FOL;
  end;

  //REGISTRO M400: RECEITAS ISENTAS, NÃO ALCANÇADAS PELA INCIDÊNCIA DA CONTRIBUIÇÃO, SUJEITAS A ALÍQUOTA ZERO OU DE VENDAS COM SUSPENSÃO - PIS/PASEP
  TRegistroM400 = class
  private
    FVL_TOT_REC           : currency;
    FDESC_COMPL           : string;
    FCOD_CTA              : string;
    FCST_PIS              : TACBrCstPis;

    FRegistroM410         : TRegistroM410List; // NIVEL 3
  public
    constructor Create;  virtual;              /// Create
    destructor  Destroy; override;             /// Destroy
    function LocalizaRegistroM410(const ANAT_REC: String;
                                  const ACOD_CTA: String='';
                                  const ADESC_COMPL : String=''): TRegistroM410;
    property CST_PIS      : TACBrCstPis read FCST_PIS      write FCST_PIS;
    property VL_TOT_REC   : currency    read FVL_TOT_REC   write FVL_TOT_REC;
    property COD_CTA      : string      read FCOD_CTA      write FCOD_CTA;
    property DESC_COMPL   : string      read FDESC_COMPL   write FDESC_COMPL;

    property RegistroM410 : TRegistroM410List    read FRegistroM410 write FRegistroM410;
  end;

  // Registro M400 - Lista
  TRegistroM400List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM400;
    procedure SetItem(Index: Integer; const Value: TRegistroM400);
  public
    function New: TRegistroM400;
    property Items[Index: Integer]: TRegistroM400 read GetItem write SetItem;
  end;

  //REGISTRO M410: DETALHAMENTO DAS RECEITAS ISENTAS, NÃO ALCANÇADAS PELA INCIDÊNCIA DA CONTRIBUIÇÃO, SUJEITAS A ALÍQUOTA ZERO OU DE VENDAS COM SUSPENSÃO - PIS/PASEP
  TRegistroM410 = class
  private
    FVL_REC             : currency;
    FDESC_COMPL         : string;
    FCOD_CTA            : string;
    FNAT_REC            : string;
  public
    property NAT_REC    : string   read FNAT_REC    write FNAT_REC;
    property VL_REC     : currency read FVL_REC     write FVL_REC;
    property COD_CTA    : string   read FCOD_CTA    write FCOD_CTA;
    property DESC_COMPL : string   read FDESC_COMPL write FDESC_COMPL;
  end;

  // Registro M410 - Lista
  TRegistroM410List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM410;
    procedure SetItem(Index: Integer; const Value: TRegistroM410);
  public
    function New: TRegistroM410;
    property Items[Index: Integer]: TRegistroM410 read GetItem write SetItem;
  end;

  //REGISTRO M500: CRÉDITO DE COFINS RELATIVO AO PERÍODO
  TRegistroM500 = class
  private
    FVL_AJUS_ACRES             : currency;
    FALIQ_COFINS               : Variant;
    FVL_CRED                   : currency;
    FVL_AJUS_REDUC             : currency;
    FVL_CRED_DESC              : Variant;
    FSLD_CRED                  : currency;
    FALIQ_COFINS_QUANT         : Variant;
    FVL_CRED_DISP              : currency;
    FVL_CRED_DIFER             : currency;
    FVL_BC_COFINS              : Variant;
    FQUANT_BC_COFINS           : Variant;
    FIND_DESC_CRED             : TACBrIndDescCred;
    FIND_CRED_ORI              : TACBrIndCredOri;
    FCOD_CRED                  : string;

    FRegistroM505              : TRegistroM505List; // NIVEL 3
    FRegistroM510              : TRegistroM510List; // NIVEL 3
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property COD_CRED          : string            read FCOD_CRED          write FCOD_CRED;
    property IND_CRED_ORI      : TACBrIndCredOri   read FIND_CRED_ORI      write FIND_CRED_ORI;
    property VL_BC_COFINS      : Variant           read FVL_BC_COFINS      write FVL_BC_COFINS;
    property ALIQ_COFINS       : Variant           read FALIQ_COFINS       write FALIQ_COFINS;
    property QUANT_BC_COFINS   : Variant           read FQUANT_BC_COFINS   write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT : Variant           read FALIQ_COFINS_QUANT write FALIQ_COFINS_QUANT;
    property VL_CRED           : currency          read FVL_CRED           write FVL_CRED;
    property VL_AJUS_ACRES     : currency          read FVL_AJUS_ACRES     write FVL_AJUS_ACRES;
    property VL_AJUS_REDUC     : currency          read FVL_AJUS_REDUC     write FVL_AJUS_REDUC;
    property VL_CRED_DIFER     : currency          read FVL_CRED_DIFER     write FVL_CRED_DIFER;
    property VL_CRED_DISP      : currency          read FVL_CRED_DISP      write FVL_CRED_DISP;
    property IND_DESC_CRED     : TACBrIndDescCred  read FIND_DESC_CRED     write FIND_DESC_CRED;
    property VL_CRED_DESC      : Variant           read FVL_CRED_DESC      write FVL_CRED_DESC;
    property SLD_CRED          : currency          read FSLD_CRED          write FSLD_CRED;

    property RegistroM505      : TRegistroM505List read FRegistroM505      write FRegistroM505;
    property RegistroM510      : TRegistroM510List read FRegistroM510      write FRegistroM510;
  end;

  // Registro M500 - Lista
  TRegistroM500List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM500;
    procedure SetItem(Index: Integer; const Value: TRegistroM500);
  public
    function New: TRegistroM500;
    property Items[Index: Integer]: TRegistroM500 read GetItem write SetItem;
  end;

  //REGISTRO M505: DETALHAMENTO DA BASE DE CALCULO DO CRÉDITO APURADO NO PERÍODO - COFINS
  TRegistroM505 = class
  private
    FQUANT_BC_COFINS             : Variant;
    FVL_BC_COFINS_CUM            : Variant;
    FQUANT_BC_COFINS_TOT         : Variant;
    FVL_BC_COFINS_TOT            : Variant;
    FVL_BC_COFINS_NC             : Variant;
    FVL_BC_COFINS                : Variant;
    FCST_COFINS                  : TACBrSituacaoTribCOFINS;
    FDESC_CRED                   : string;
    FNAT_BC_CRED                 : TACBrBaseCalculoCredito;
  public
    property NAT_BC_CRED         : TACBrBaseCalculoCredito read FNAT_BC_CRED         write FNAT_BC_CRED;
    property CST_COFINS          : TACBrSituacaoTribCOFINS read FCST_COFINS          write FCST_COFINS;
    property VL_BC_COFINS_TOT    : Variant                 read FVL_BC_COFINS_TOT    write FVL_BC_COFINS_TOT;
    property VL_BC_COFINS_CUM    : Variant                 read FVL_BC_COFINS_CUM    write FVL_BC_COFINS_CUM;
    property VL_BC_COFINS_NC     : Variant                 read FVL_BC_COFINS_NC     write FVL_BC_COFINS_NC;
    property VL_BC_COFINS        : Variant                 read FVL_BC_COFINS        write FVL_BC_COFINS;
    property QUANT_BC_COFINS_TOT : Variant                 read FQUANT_BC_COFINS_TOT write FQUANT_BC_COFINS_TOT;
    property QUANT_BC_COFINS     : Variant                 read FQUANT_BC_COFINS     write FQUANT_BC_COFINS;
    property DESC_CRED           : string                  read FDESC_CRED           write FDESC_CRED;
  end;

  // Registro M505 - Lista
  TRegistroM505List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM505;
    procedure SetItem(Index: Integer; const Value: TRegistroM505);
  public
    function New: TRegistroM505;
    property Items[Index: Integer]: TRegistroM505 read GetItem write SetItem;
  end;

  //REGISTRO M510: AJUSTES DO CRÉDITO DE COFINS APURADO
  TRegistroM510 = class
  private
    FRegistroM515: TRegistroM515List;
    FVL_AJ            : currency;
    FNUM_DOC          : string;
    FCOD_AJ           : TACBrCodAj;
    FDESCR_AJ         : string;
    FIND_AJ           : TACBrIndAJ;
    FDT_REF           : TDateTime;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property IND_AJ   : TACBrIndAJ    read FIND_AJ   write FIND_AJ;
    property VL_AJ    : currency      read FVL_AJ    write FVL_AJ;
    property COD_AJ   : TACBrCodAj    read FCOD_AJ   write FCOD_AJ;
    property NUM_DOC  : string        read FNUM_DOC  write FNUM_DOC;
    property DESCR_AJ : string        read FDESCR_AJ write FDESCR_AJ;
    property DT_REF   : TDateTime     read FDT_REF   write FDT_REF;

    property RegistroM515      : TRegistroM515List read FRegistroM515      write FRegistroM515;
  end;

  // Registro M510 - Lista
  TRegistroM510List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM510;
    procedure SetItem(Index: Integer; const Value: TRegistroM510);
  public
    function New: TRegistroM510;
    property Items[Index: Integer]: TRegistroM510 read GetItem write SetItem;
  end;

  //REGISTRO 515: DETALHAMENTO DOS AJUSTES DO CRÉDITO DE COFINS APURADO
  TRegistroM515 = class
  private
    FCOD_CTA: string;
    FCST_COFINS: TACBrSituacaoTribCOFINS;
    FDESC_AJ: string;
    FDET_ALIQ: Currency;
    FDET_BC_CRED: Currency;
    FDET_VALOR_AJ: Currency;
    FDT_OPER_AJ: TDateTime;
    FINFO_COMPL: string;
  public
    property DET_VALOR_AJ : Currency read FDET_VALOR_AJ write FDET_VALOR_AJ;
    property CST_COFINS   : TACBrCstCofins read FCST_COFINS write FCST_COFINS;
    property DET_BC_CRED  : Currency read FDET_BC_CRED write FDET_BC_CRED;
    property DET_ALIQ     : Currency read FDET_ALIQ write FDET_ALIQ;
    property DT_OPER_AJ   : TDateTime  read FDT_OPER_AJ write FDT_OPER_AJ;
    property DESC_AJ      : string read FDESC_AJ write FDESC_AJ;
    property COD_CTA      : string read FCOD_CTA write FCOD_CTA;
    property INFO_COMPL   : string read FINFO_COMPL write FINFO_COMPL;
  end;

  // Registro M515 - Lista
  TRegistroM515List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM515;
    procedure SetItem(Index: Integer; const Value: TRegistroM515);
  public
    function New: TRegistroM515;
    property Items[Index: Integer]: TRegistroM515 read GetItem write SetItem;
  end;

  //REGISTRO M600: CONSOLIDAÇÃO DA CONTRIBUIÇÃO PARA A SEGURIDADE SOCIAL - COFINS DO PERÍODO
  TRegistroM600 = class
  private
    FRegistroM605: TRegistroM605List;
    FVL_TOT_CONT_REC              : currency;
    FVL_TOT_CRED_DESC             : currency;
    FVL_OUT_DED_CUM               : currency;
    FVL_TOT_CONT_NC_DEV           : currency;
    FVL_RET_CUM                   : currency;
    FVL_RET_NC                    : currency;
    FVL_TOT_CONT_NC_PER           : currency;
    FVL_CONT_CUM_REC              : currency;
    FVL_CONT_NC_REC               : currency;
    FVL_OUT_DED_NC                : currency;
    FVL_TOT_CRED_DESC_ANT         : currency;
    FVL_TOT_CONT_CUM_PER          : currency;

    FRegistroM610                 : TRegistroM610List; // NIVEL 3
  public
    constructor Create; virtual;                       /// Create
    destructor Destroy; override;                      /// Destroy

    property VL_TOT_CONT_NC_PER   : currency          read FVL_TOT_CONT_NC_PER   write FVL_TOT_CONT_NC_PER;
    property VL_TOT_CRED_DESC     : currency          read FVL_TOT_CRED_DESC     write FVL_TOT_CRED_DESC;
    property VL_TOT_CRED_DESC_ANT : currency          read FVL_TOT_CRED_DESC_ANT write FVL_TOT_CRED_DESC_ANT;
    property VL_TOT_CONT_NC_DEV   : currency          read FVL_TOT_CONT_NC_DEV   write FVL_TOT_CONT_NC_DEV;
    property VL_RET_NC            : currency          read FVL_RET_NC            write FVL_RET_NC;
    property VL_OUT_DED_NC        : currency          read FVL_OUT_DED_NC        write FVL_OUT_DED_NC;
    property VL_CONT_NC_REC       : currency          read FVL_CONT_NC_REC       write FVL_CONT_NC_REC;
    property VL_TOT_CONT_CUM_PER  : currency          read FVL_TOT_CONT_CUM_PER  write FVL_TOT_CONT_CUM_PER;
    property VL_RET_CUM           : currency          read FVL_RET_CUM           write FVL_RET_CUM;
    property VL_OUT_DED_CUM       : currency          read FVL_OUT_DED_CUM       write FVL_OUT_DED_CUM;
    property VL_CONT_CUM_REC      : currency          read FVL_CONT_CUM_REC      write FVL_CONT_CUM_REC;
    property VL_TOT_CONT_REC      : currency          read FVL_TOT_CONT_REC      write FVL_TOT_CONT_REC;

    property RegistroM605         : TRegistroM605List read FRegistroM605         write FRegistroM605;
    property RegistroM610         : TRegistroM610List read FRegistroM610         write FRegistroM610;
  end;

  //REGISTRO M605: COFINS A RECOLHER – DETALHAMENTO POR CÓDIGO DE RECEITA
  TRegistroM605 = class
  private
    FCOD_REC: string;
    FNUM_CAMPO: string;
    FVL_DEBITO: Currency;
  public
    property NUM_CAMPO : string   read FNUM_CAMPO write FNUM_CAMPO;
    property COD_REC   : string   read FCOD_REC   write FCOD_REC;
    property VL_DEBITO : Currency read FVL_DEBITO write FVL_DEBITO;
  end;

  // Registro M605 - Lista
  TRegistroM605List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM605;
    procedure SetItem(Index: Integer; const Value: TRegistroM605);
  public
    function New: TRegistroM605;
    property Items[Index: Integer]: TRegistroM605 read GetItem write SetItem;
  end;

  //REGISTRO M610: DETALHAMENTO DA CONTRIBUIÇÃO PARA A SEGURIDADE SOCIAL - COFINS DO PERÍODO
  TRegistroM610 = class
  private
    FVL_CONT_DIFER             : Variant;
    FVL_CONT_PER               : Currency;
    FALIQ_COFINS               : Variant;
    FVL_AJUS_REDUC             : currency;
    FVL_AJUS_ACRES             : currency;
    FVL_CONT_DIFER_ANT         : Variant;
    FALIQ_COFINS_QUANT         : Variant;
    FVL_BC_CONT                : currency;
    FVL_CONT_APUR              : currency;
    FVL_REC_BRT                : currency;
    FQUANT_BC_COFINS           : Variant;
    FCOD_CONT                  : TACBrCodCont;
    FVL_AJUS_ACRES_BC_COFINS   : currency;
    FVL_AJUS_REDUC_BC_COFINS   : currency;
    FVL_BC_CONT_AJUS           : currency;

    FRegistroM611              : TRegistroM611;     // NIVEL 4
    FRegistroM615              : TRegistroM615;     // NIVEL 4
    FRegistroM620              : TRegistroM620List; // NIVEL 4
    FRegistroM630              : TRegistroM630List; // NIVEL 4
  public
    constructor Create;  virtual;                   /// Create
    destructor  Destroy; override;                  /// Destroy

    property COD_CONT                : TACBrCodCont      read FCOD_CONT                write FCOD_CONT;
    property VL_REC_BRT              : currency          read FVL_REC_BRT              write FVL_REC_BRT;
    property VL_BC_CONT              : currency          read FVL_BC_CONT              write FVL_BC_CONT;
    property VL_AJUS_ACRES_BC_COFINS : currency          read FVL_AJUS_ACRES_BC_COFINS write FVL_AJUS_ACRES_BC_COFINS;
    property VL_AJUS_REDUC_BC_COFINS : currency          read FVL_AJUS_REDUC_BC_COFINS write FVL_AJUS_REDUC_BC_COFINS;
    property VL_BC_CONT_AJUS         : currency          read FVL_BC_CONT_AJUS         write FVL_BC_CONT_AJUS;
    property ALIQ_COFINS             : Variant           read FALIQ_COFINS             write FALIQ_COFINS;
    property QUANT_BC_COFINS         : Variant           read FQUANT_BC_COFINS         write FQUANT_BC_COFINS;
    property ALIQ_COFINS_QUANT       : Variant           read FALIQ_COFINS_QUANT       write FALIQ_COFINS_QUANT;
    property VL_CONT_APUR            : currency          read FVL_CONT_APUR            write FVL_CONT_APUR;
    property VL_AJUS_ACRES           : currency          read FVL_AJUS_ACRES           write FVL_AJUS_ACRES;
    property VL_AJUS_REDUC           : currency          read FVL_AJUS_REDUC           write FVL_AJUS_REDUC;
    property VL_CONT_DIFER           : Variant           read FVL_CONT_DIFER           write FVL_CONT_DIFER;
    property VL_CONT_DIFER_ANT       : Variant           read FVL_CONT_DIFER_ANT       write FVL_CONT_DIFER_ANT;
    property VL_CONT_PER             : currency          read FVL_CONT_PER             write FVL_CONT_PER;

    property RegistroM611            : TRegistroM611     read FRegistroM611            write FRegistroM611;
    property RegistroM615            : TRegistroM615     read FRegistroM615            write FRegistroM615;
    property RegistroM620            : TRegistroM620List read FRegistroM620            write FRegistroM620;
    property RegistroM630            : TRegistroM630List read FRegistroM630            write FRegistroM630;
  end;

  // Registro M610 - Lista
  TRegistroM610List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM610;
    procedure SetItem(Index: Integer; const Value: TRegistroM610);
  public
    function New: TRegistroM610;
    property Items[Index: Integer]: TRegistroM610 read GetItem write SetItem;
  end;

  //REGISTRO M611: SOCIEDADES COOPERATIVAS - COMPOSIÇÃO DA BASE DE CALCULO - COFINS
  TRegistroM611 = class
  private
    FVL_BC_CONT_ANT_EXC_COOP         : currency;
    FVL_EXC_COOP_GER                 : currency;
    FVL_BC_CONT                      : currency;
    FVL_EXC_ESP_COOP                 : currency;
    FIND_TIP_COOP                    : TACBrIndTipCoop;
  public
    property IND_TIP_COOP            : TACBrIndTipCoop   read FIND_TIP_COOP            write FIND_TIP_COOP;
    property VL_BC_CONT_ANT_EXC_COOP : currency          read FVL_BC_CONT_ANT_EXC_COOP write FVL_BC_CONT_ANT_EXC_COOP;
    property VL_EXC_COOP_GER         : currency          read FVL_EXC_COOP_GER         write FVL_EXC_COOP_GER;
    property VL_EXC_ESP_COOP         : currency          read FVL_EXC_ESP_COOP         write FVL_EXC_ESP_COOP;
    property VL_BC_CONT              : currency          read FVL_BC_CONT              write FVL_BC_CONT;
  end;


  //REGISTRO M615:  Ajustes da Base de Cálculo da COFINS Apurada
  { TRegistroM615 }
  TRegistroM615 = class
  private
    FVL_AJ_BC             : currency;
    FCNPJ                 : string;
    FDT_REF               : TDateTime;
    FDESCR_AJ_BC          : string;
    FIND_AJ_BC            : TACBrIndAJ;
    FNUM_DOC              : string;
    FINFO_COMPL           : string;
    FCOD_AJ_BC            : TACBrTabCodAjBaseCalcContrib;
    FCOD_CTA              : string;
  public
    property IND_AJ_BC    : TACBrIndAJ                    read FIND_AJ_BC       write FIND_AJ_BC;
    property VL_AJ_BC     : currency                      read FVL_AJ_BC        write FVL_AJ_BC;
    property COD_AJ_BC    : TACBrTabCodAjBaseCalcContrib  read FCOD_AJ_BC       write FCOD_AJ_BC;
    property NUM_DOC      : string                        read FNUM_DOC         write FNUM_DOC;
    property DESCR_AJ_BC  : string                        read FDESCR_AJ_BC     write FDESCR_AJ_BC;
    property DT_REF       : TDateTime                     read FDT_REF          write FDT_REF;
    property COD_CTA      : string                        read FCOD_CTA         write FCOD_CTA;
    property CNPJ         : string                        read FCNPJ            write FCNPJ;
    property INFO_COMPL   : string                        read FINFO_COMPL      write FINFO_COMPL;
  end;




  //REGISTRO M620: AJUSTES DA COFINS APURADA

  { TRegistroM620 }

  TRegistroM620 = class
  private
    FRegistroM625     : TRegistroM625List;
    FVL_AJ            : currency;
    FNUM_DOC          : string;
    FCOD_AJ           : TACBrCodAj;
    FDESCR_AJ         : string;
    FIND_AJ           : TACBrIndAJ;
    FDT_REF           : TDateTime;
  public
    constructor Create;  virtual;                   /// Create
    destructor  Destroy; override;                  /// Destroy

    property IND_AJ   : TACBrIndAJ    read FIND_AJ   write FIND_AJ;
    property VL_AJ    : currency      read FVL_AJ    write FVL_AJ;
    property COD_AJ   : TACBrCodAj    read FCOD_AJ   write FCOD_AJ;
    property NUM_DOC  : string        read FNUM_DOC  write FNUM_DOC;
    property DESCR_AJ : string        read FDESCR_AJ write FDESCR_AJ;
    property DT_REF   : TDateTime     read FDT_REF   write FDT_REF;

    property RegistroM625      : TRegistroM625List read FRegistroM625      write FRegistroM625;
  end;

  // Registro M620 - Lista
  TRegistroM620List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM620;
    procedure SetItem(Index: Integer; const Value: TRegistroM620);
  public
    function New: TRegistroM620;
    property Items[Index: Integer]: TRegistroM620 read GetItem write SetItem;
  end;

  //REGISTRO M625: DETALHAMENTO DOS AJUSTES DA COFINS APURADA
  TRegistroM625 = class
  private
    FCOD_CTA: string;
    FCST_COFINS: TACBrCstCofins;
    FDESC_AJ: string;
    FDET_ALIQ: Currency;
    FDET_BC_CRED: Currency;
    FDET_VALOR_AJ: Currency;
    FDT_OPER_AJ: TDateTime;
    FINFO_COMPL: string;
  public
    property DET_VALOR_AJ : Currency  read FDET_VALOR_AJ write FDET_VALOR_AJ;
    property CST_COFINS   : TACBrCstCofins read FCST_COFINS   write FCST_COFINS;
    property DET_BC_CRED  : Currency  read FDET_BC_CRED  write FDET_BC_CRED;
    property DET_ALIQ     : Currency  read FDET_ALIQ     write FDET_ALIQ;
    property DT_OPER_AJ   : TDateTime read FDT_OPER_AJ   write FDT_OPER_AJ;
    property DESC_AJ      : string    read FDESC_AJ      write FDESC_AJ;
    property COD_CTA      : string    read FCOD_CTA      write FCOD_CTA;
    property INFO_COMPL   : string    read FINFO_COMPL   write FINFO_COMPL;
  end;

  // Registro M625 - Lista
  TRegistroM625List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM625;
    procedure SetItem(Index: Integer; const Value: TRegistroM625);
  public
    function New: TRegistroM625;
    property Items[Index: Integer]: TRegistroM625 read GetItem write SetItem;
  end;

  //REGISTRO M630: INFORMAÇÕES ADICIONAIS DE DIFERIMENTO
  TRegistroM630 = class
  private
    FVL_CONT_DIF          : currency;
    FVL_NAO_RECEB         : currency;
    FVL_CRED_DIF          : currency;
    FVL_VEND              : currency;
    FCNPJ                 : string;
    FCOD_CRED             : string;
  public
    property CNPJ         : string       read FCNPJ         write FCNPJ;
    property VL_VEND      : currency     read FVL_VEND      write FVL_VEND;
    property VL_NAO_RECEB : currency     read FVL_NAO_RECEB write FVL_NAO_RECEB;
    property VL_CONT_DIF  : currency     read FVL_CONT_DIF  write FVL_CONT_DIF;
    property VL_CRED_DIF  : currency     read FVL_CRED_DIF  write FVL_CRED_DIF;
    property COD_CRED     : string       read FCOD_CRED     write FCOD_CRED;
  end;

  // Registro M630 - Lista
  TRegistroM630List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM630;
    procedure SetItem(Index: Integer; const Value: TRegistroM630);
  public
    function New: TRegistroM630;
    property Items[Index: Integer]: TRegistroM630 read GetItem write SetItem;
  end;


  //REGISTRO M700: COFINS DIFERIDA EM PERÍODOS ANTERIORES - VALORES A PAGAR NO PERÍODO
  TRegistroM700 = class
  private
    FCOD_CONT                   : TACBrCodCont;           {COD_CONT}
    FVL_CONT_APUR_DIFER         : currency;               {VL_CONT_APUR_DIFER}
    FNAT_CRED_DESC              : TACBrNatCredDesc;       {NAT_CRED_DESC}
    FVL_CRED_DESC_DIFER         : currency;               {VL_CRED_DESC_DIFER}
    FVL_CONT_DIFER_ANT          : currency;               {VL_CONT_DIFER_ANT}
    FPER_APUR                   : string;                 {PER_APUR}
    FDT_RECEB                   : TDateTime;              {DT_RECEB}
  public
    property COD_CONT           : TACBrCodCont     read FCOD_CONT             write FCOD_CONT;
    property VL_CONT_APUR_DIFER : currency         read FVL_CONT_APUR_DIFER   write FVL_CONT_APUR_DIFER;
    property NAT_CRED_DESC      : TACBrNatCredDesc read FNAT_CRED_DESC        write FNAT_CRED_DESC;
    property VL_CRED_DESC_DIFER : currency         read FVL_CRED_DESC_DIFER   write FVL_CRED_DESC_DIFER;
    property VL_CONT_DIFER_ANT  : currency         read FVL_CONT_DIFER_ANT    write FVL_CONT_DIFER_ANT;
    property PER_APUR           : string           read FPER_APUR             write FPER_APUR;
    property DT_RECEB           : TDateTime        read FDT_RECEB             write FDT_RECEB;
  end;

  // Registro M700 - Lista
  TRegistroM700List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM700;
    procedure SetItem(Index: Integer; const Value: TRegistroM700);
  public
    function New: TRegistroM700;
    property Items[Index: Integer]: TRegistroM700 read GetItem write SetItem;
  end;

  //REGISTRO M800: RECEITAS ISENTAS, NÃO ALCANÇADAS PELA INCIDÊNCIA DA CONTRIBUIÇÃO, SUJEITAS A ALÍQUOTA ZERO OU DE VENDAS COM SUSPENSÃO - COFINS
  TRegistroM800 = class
  private
    FVL_TOT_REC           : currency;
    FDESC_COMPL           : string;
    FCOD_CTA              : string;
    FCST_COFINS           : TACBrSituacaoTribCOFINS;

    FRegistroM810         : TRegistroM810List; // NIVEL 3
  public
    constructor Create; virtual;               /// Create
    destructor Destroy; override;              /// Destroy
    function LocalizaRegistroM810(const ANAT_REC: String;
                                  const ACOD_CTA: String='';
                                  const ADESC_COMPL : String=''): TRegistroM810;
    property CST_COFINS   : TACBrSituacaoTribCOFINS read FCST_COFINS   write FCST_COFINS;
    property VL_TOT_REC   : currency                read FVL_TOT_REC   write FVL_TOT_REC;
    property COD_CTA      : string                  read FCOD_CTA      write FCOD_CTA;
    property DESC_COMPL   : string                  read FDESC_COMPL   write FDESC_COMPL;

    property RegistroM810 : TRegistroM810List       read FRegistroM810 write FRegistroM810;
  end;

  // Registro M800 - Lista
  TRegistroM800List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM800;
    procedure SetItem(Index: Integer; const Value: TRegistroM800);
  public
    function New: TRegistroM800;
    property Items[Index: Integer]: TRegistroM800 read GetItem write SetItem;
  end;

  //REGISTRO M810: DETALHAMENTO DAS RECEITAS ISENTAS, NÃO ALCANÇADAS PELA INCIDÊNCIA DA CONTRIBUIÇÃO, SUJEITAS A ALÍQUOTA ZERO OU DE VENDAS COM SUSPENSÃO - COFINS
  TRegistroM810 = class
  private
    FVL_REC             : currency;
    FDESC_COMPL         : string;
    FCOD_CTA            : string;
    FNAT_REC            : string;
  public
    property NAT_REC    : string   read FNAT_REC    write FNAT_REC;
    property VL_REC     : currency read FVL_REC     write FVL_REC;
    property COD_CTA    : string   read FCOD_CTA    write FCOD_CTA;
    property DESC_COMPL : string   read FDESC_COMPL write FDESC_COMPL;
  end;

  // Registro M810 - Lista
  TRegistroM810List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroM810;
    procedure SetItem(Index: Integer; const Value: TRegistroM810);
  public
    function New: TRegistroM810;
    property Items[Index: Integer]: TRegistroM810 read GetItem write SetItem;
  end;

  //REGISTRO M900: ENCERRAMENTO DO BLOCO M
  TRegistroM990 = class
  private
    FQTD_LIN_M: integer;
  public
    property QTD_LIN_M: integer read FQTD_LIN_M write FQTD_LIN_M;
  end;

implementation

{ TRegistroM620 }

constructor TRegistroM620.Create;
begin
  FRegistroM625 := TRegistroM625List.Create;
end;

destructor TRegistroM620.Destroy;
begin
  FRegistroM625.Free;
  inherited;
end;

{ TRegistroM625List }

function TRegistroM625List.GetItem(Index: Integer): TRegistroM625;
begin
  Result := TRegistroM625(Inherited Items[Index]);
end;

procedure TRegistroM625List.SetItem(Index: Integer; const Value: TRegistroM625);
begin
  Put(Index, Value);
end;

function TRegistroM625List.New: TRegistroM625;
begin
  Result := TRegistroM625.Create;
  Add(Result);
end;

{ TRegistroM605List }

function TRegistroM605List.GetItem(Index: Integer): TRegistroM605;
begin
  Result := TRegistroM605(Inherited Items[Index]);
end;

procedure TRegistroM605List.SetItem(Index: Integer; const Value: TRegistroM605);
begin
  Put(Index, Value);
end;

function TRegistroM605List.New: TRegistroM605;
begin
  Result := TRegistroM605.Create;
  Add(Result);
end;

{ TRegistroM510 }

constructor TRegistroM510.Create;
begin
  FRegistroM515 := TRegistroM515List.Create;
end;

destructor TRegistroM510.Destroy;
begin
  FRegistroM515.Free;
  inherited;
end;

{ TRegistroM515List }

function TRegistroM515List.GetItem(Index: Integer): TRegistroM515;
begin
  Result := TRegistroM515(Inherited Items[Index]);
end;

procedure TRegistroM515List.SetItem(Index: Integer; const Value: TRegistroM515);
begin
  Put(Index, Value);
end;

function TRegistroM515List.New: TRegistroM515;
begin
  Result := TRegistroM515.Create;
  Add(Result);
end;

{ TRegistroM220 }

constructor TRegistroM220.Create;
begin
  FRegistroM225 := TRegistroM225List.Create;
end;

destructor TRegistroM220.Destroy;
begin
  FRegistroM225.Free;
  inherited;
end;

{ TRegistroM225List }

function TRegistroM225List.GetItem(Index: Integer): TRegistroM225;
begin
  Result := TRegistroM225(Inherited Items[Index]);
end;

procedure TRegistroM225List.SetItem(Index: Integer; const Value: TRegistroM225);
begin
  Put(Index, Value);
end;

function TRegistroM225List.New: TRegistroM225;
begin
  Result := TRegistroM225.Create;
  Add(Result);
end;

{ TRegistroM110 }

constructor TRegistroM110.Create;
begin
  FRegistroM115 := TRegistroM115List.Create;
end;

destructor TRegistroM110.Destroy;
begin
  FRegistroM115.Destroy;
  inherited;
end;

{ TRegistroM115List }

function TRegistroM115List.GetItem(Index: Integer): TRegistroM115;
begin
  Result := TRegistroM115(Inherited Items[Index]);
end;

procedure TRegistroM115List.SetItem(Index: Integer; const Value: TRegistroM115);
begin
  Put(Index, Value);
end;

function TRegistroM115List.New: TRegistroM115;
begin
  Result := TRegistroM115.Create;
  Add(Result);
end;

{ TRegistroM205List }

function TRegistroM205List.GetItem(Index: Integer): TRegistroM205;
begin
  Result := TRegistroM205(Inherited Items[Index]);
end;

procedure TRegistroM205List.SetItem(Index: Integer; const Value: TRegistroM205);
begin
  Put(Index, Value);
end;

function TRegistroM205List.New: TRegistroM205;
begin
  Result := TRegistroM205.Create;
  Add(Result);
end;


{TRegistroM001}
constructor TRegistroM001.Create;
begin
  inherited Create;
  FRegistroM100 := TRegistroM100List.Create;
  FRegistroM200 := TRegistroM200.Create;
  FRegistroM300 := TRegistroM300List.Create;
  FRegistroM350 := TRegistroM350.Create;
  FRegistroM400 := TRegistroM400List.Create;
  FRegistroM500 := TRegistroM500List.Create;
  FRegistroM600 := TRegistroM600.Create;
  FRegistroM700 := TRegistroM700List.Create;
  FRegistroM800 := TRegistroM800List.Create;
end;

destructor TRegistroM001.Destroy;
begin
  FRegistroM100.Free;
  FRegistroM200.Free;
  FRegistroM300.Free;
  FRegistroM350.Free;
  FRegistroM400.Free;
  FRegistroM500.Free;
  FRegistroM600.Free;
  FRegistroM700.Free;
  FRegistroM800.Free;
  inherited;
end;

function TRegistroM001.LocalizaRegistroM400(ACST_PIS: TACBrCstPis;
  const ACOD_CTA: String=''; const ADESC_COMPL : String=''): TRegistroM400;
 var
  I : Integer;
  VReg : TRegistroM400;
begin
  result := nil;
  for I := 0 to pred( FRegistroM400.Count ) do
   begin
    VReg := FRegistroM400.Items[I];
    if ((VReg.CST_PIS = ACST_PIS) and
        (VReg.COD_CTA = ACOD_CTA) and
        (VReg.DESC_COMPL = ADESC_COMPL)) then
     begin
      result := VReg;
      Break;
     end;
   end;
end;

function TRegistroM800.LocalizaRegistroM810(const ANAT_REC: String;
                                            const ACOD_CTA: String='';
                                            const ADESC_COMPL : String=''): TRegistroM810;
 var
  I : Integer;
  VReg : TRegistroM810;
begin
  result := nil;
  for I := 0 to pred( FRegistroM810.Count ) do
   begin
    VReg := FRegistroM810.Items[I];
    if ((VReg.NAT_REC = ANAT_REC) and
        (VReg.COD_CTA = ACOD_CTA) and
        (VReg.DESC_COMPL = ADESC_COMPL)) then
     begin
      result := VReg;
      Break;
     end;
   end;
end;

function TRegistroM001.LocalizaRegistroM800(ACST_COFINS: TACBrCstCofins;
  const ACOD_CTA: String=''; const ADESC_COMPL : String=''): TRegistroM800;
 var
  I : Integer;
  VReg : TRegistroM800;
begin
  result := nil;
  for I := 0 to pred( FRegistroM800.Count ) do
   begin
    VReg := FRegistroM800.Items[I];
    if ((VReg.CST_COFINS = ACST_COFINS) and (VReg.COD_CTA = ACOD_CTA) and
     (VReg.DESC_COMPL = ADESC_COMPL)) then
     begin
      result := VReg;
      Break;
     end;
   end;
end;

{TRegistroM100}

function TRegistroM100List.GetItem(Index: Integer): TRegistroM100;
begin
  Result := TRegistroM100(Inherited Items[Index]);
end;

function TRegistroM100List.New: TRegistroM100;
begin
  Result := TRegistroM100.Create;
  Add(Result);
end;

procedure TRegistroM100List.SetItem(Index: Integer; const Value: TRegistroM100);
begin
  Put(Index, Value);
end;

{ TRegistroM100 }

constructor TRegistroM100.Create;
begin
  FRegistroM105 := TRegistroM105List.Create;
  FRegistroM110 := TRegistroM110List.Create;
end;

destructor TRegistroM100.Destroy;
begin
  FRegistroM105.Free;
  FRegistroM110.Free;
  inherited;
end;

{TRegistroM105}

function TRegistroM105List.GetItem(Index: Integer): TRegistroM105;
begin
  Result := TRegistroM105(Inherited Items[Index]);
end;

function TRegistroM105List.New: TRegistroM105;
begin
  Result := TRegistroM105.Create;
  Add(Result);
end;

procedure TRegistroM105List.SetItem(Index: Integer; const Value: TRegistroM105);
begin
  Put(Index, Value);
end;

{TRegistroM110}

function TRegistroM110List.GetItem(Index: Integer): TRegistroM110;
begin
  Result := TRegistroM110(Inherited Items[Index]);
end;

function TRegistroM110List.New: TRegistroM110;
begin
  Result := TRegistroM110.Create;
  Add(Result);
end;

procedure TRegistroM110List.SetItem(Index: Integer; const Value: TRegistroM110);
begin
  Put(Index, Value);
end;

{TRegistroM210}

function TRegistroM210List.GetItem(Index: Integer): TRegistroM210;
begin
  Result := TRegistroM210(Inherited Items[Index]);
end;

function TRegistroM210List.New: TRegistroM210;
begin
  Result := TRegistroM210.Create;
  Add(Result);
end;

procedure TRegistroM210List.SetItem(Index: Integer; const Value: TRegistroM210);
begin
  Put(Index, Value);
end;

{ TRegistroM210 }

constructor TRegistroM210.Create;
begin
  FRegistroM211 := TRegistroM211.Create;
  FRegistroM215 := TRegistroM215.Create;
  FRegistroM220 := TRegistroM220List.Create;
  FRegistroM230 := TRegistroM230List.Create;
end;

destructor TRegistroM210.Destroy;
begin
  FRegistroM211.Free;
  FRegistroM215.Free;
  FRegistroM220.Free;
  FRegistroM230.Free;
  inherited;
end;

{TRegistroM220}

function TRegistroM220List.GetItem(Index: Integer): TRegistroM220;
begin
  Result := TRegistroM220(Inherited Items[Index]);
end;

function TRegistroM220List.New: TRegistroM220;
begin
  Result := TRegistroM220.Create;
  Add(Result);
end;

procedure TRegistroM220List.SetItem(Index: Integer; const Value: TRegistroM220);
begin
  Put(Index, Value);
end;

{TRegistroM230}

function TRegistroM230List.GetItem(Index: Integer): TRegistroM230;
begin
  Result := TRegistroM230(Inherited Items[Index]);
end;

function TRegistroM230List.New: TRegistroM230;
begin
  Result := TRegistroM230.Create;
  Add(Result);
end;

procedure TRegistroM230List.SetItem(Index: Integer; const Value: TRegistroM230);
begin
  Put(Index, Value);
end;

{TRegistroM300}

function TRegistroM300List.GetItem(Index: Integer): TRegistroM300;
begin
  Result := TRegistroM300(Inherited Items[Index]);
end;

function TRegistroM300List.New: TRegistroM300;
begin
  Result := TRegistroM300.Create;
  Add(Result);
end;

procedure TRegistroM300List.SetItem(Index: Integer; const Value: TRegistroM300);
begin
  Put(Index, Value);
end;

{TRegistroM400}

function TRegistroM400List.GetItem(Index: Integer): TRegistroM400;
begin
  Result := TRegistroM400(Inherited Items[Index]);
end;

function TRegistroM400List.New: TRegistroM400;
begin
  Result := TRegistroM400.Create;
  Add(Result);
end;

procedure TRegistroM400List.SetItem(Index: Integer; const Value: TRegistroM400);
begin
  Put(Index, Value);
end;

{ TRegistroM400 }

constructor TRegistroM400.Create;
begin
  FRegistroM410 := TRegistroM410List.Create;
end;

destructor TRegistroM400.Destroy;
begin
  FRegistroM410.Free;
  inherited;
end;

function TRegistroM400.LocalizaRegistroM410(const ANAT_REC: String;
                                            const ACOD_CTA: String='';
                                            const ADESC_COMPL : String=''): TRegistroM410;
 var
  I : Integer;
  VReg : TRegistroM410;
begin
  result := nil;
  for I := 0 to pred( FRegistroM410.Count ) do
   begin
    VReg := FRegistroM410.Items[I];
    if ((VReg.NAT_REC = ANAT_REC) and
        (VReg.COD_CTA = ACOD_CTA) and
        (VReg.DESC_COMPL = ADESC_COMPL)) then
     begin
      result := VReg;
      Break;
     end;
   end;
end;

{TRegistroM410}

function TRegistroM410List.GetItem(Index: Integer): TRegistroM410;
begin
  Result := TRegistroM410(Inherited Items[Index]);
end;

function TRegistroM410List.New: TRegistroM410;
begin
  Result := TRegistroM410.Create;
  Add(Result);
end;

procedure TRegistroM410List.SetItem(Index: Integer; const Value: TRegistroM410);
begin
  Put(Index, Value);
end;

{TRegistroM500}

function TRegistroM500List.GetItem(Index: Integer): TRegistroM500;
begin
  Result := TRegistroM500(Inherited Items[Index]);
end;

function TRegistroM500List.New: TRegistroM500;
begin
  Result := TRegistroM500.Create;
  Add(Result);
end;

procedure TRegistroM500List.SetItem(Index: Integer; const Value: TRegistroM500);
begin
  Put(Index, Value);
end;

{ TRegistroM500 }

constructor TRegistroM500.Create;
begin
  FRegistroM505 := TRegistroM505List.Create;
  FRegistroM510 := TRegistroM510List.Create;
end;

destructor TRegistroM500.Destroy;
begin
  FRegistroM505.Free;
  FRegistroM510.Free;
  inherited;
end;

{TRegistroM505}

function TRegistroM505List.GetItem(Index: Integer): TRegistroM505;
begin
  Result := TRegistroM505(Inherited Items[Index]);
end;

function TRegistroM505List.New: TRegistroM505;
begin
  Result := TRegistroM505.Create;
  Add(Result);
end;

procedure TRegistroM505List.SetItem(Index: Integer; const Value: TRegistroM505);
begin
  Put(Index, Value);
end;

{TRegistroM510}

function TRegistroM510List.GetItem(Index: Integer): TRegistroM510;
begin
  Result := TRegistroM510(Inherited Items[Index]);
end;

function TRegistroM510List.New: TRegistroM510;
begin
  Result := TRegistroM510.Create;
  Add(Result);
end;

procedure TRegistroM510List.SetItem(Index: Integer; const Value: TRegistroM510);
begin
  Put(Index, Value);
end;

{TRegistroM610}

function TRegistroM610List.GetItem(Index: Integer): TRegistroM610;
begin
  Result := TRegistroM610(Inherited Items[Index]);
end;

function TRegistroM610List.New: TRegistroM610;
begin
  Result := TRegistroM610.Create;
  Add(Result);
end;

procedure TRegistroM610List.SetItem(Index: Integer; const Value: TRegistroM610);
begin
  Put(Index, Value);
end;

{ TRegistroM610 }

constructor TRegistroM610.Create;
begin
  FRegistroM611 := TRegistroM611.Create;
  FRegistroM615 := TRegistroM615.Create;
  FRegistroM620 := TRegistroM620List.Create;
  FRegistroM630 := TRegistroM630List.Create;
end;

destructor TRegistroM610.Destroy;
begin
  FRegistroM611.Free;
  FRegistroM615.Free;
  FRegistroM620.Free;
  FRegistroM630.Free;
  inherited;
end;

{TRegistroM620}

function TRegistroM620List.GetItem(Index: Integer): TRegistroM620;
begin
  Result := TRegistroM620(Inherited Items[Index]);
end;

function TRegistroM620List.New: TRegistroM620;
begin
  Result := TRegistroM620.Create;
  Add(Result);
end;

procedure TRegistroM620List.SetItem(Index: Integer; const Value: TRegistroM620);
begin
  Put(Index, Value);
end;

{TRegistroM630}

function TRegistroM630List.GetItem(Index: Integer): TRegistroM630;
begin
  Result := TRegistroM630(Inherited Items[Index]);
end;

function TRegistroM630List.New: TRegistroM630;
begin
  Result := TRegistroM630.Create;
  Add(Result);
end;

procedure TRegistroM630List.SetItem(Index: Integer; const Value: TRegistroM630);
begin
  Put(Index, Value);
end;

{TRegistroM700}

function TRegistroM700List.GetItem(Index: Integer): TRegistroM700;
begin
  Result := TRegistroM700(Inherited Items[Index]);
end;

function TRegistroM700List.New: TRegistroM700;
begin
  Result := TRegistroM700.Create;
  Add(Result);
end;

procedure TRegistroM700List.SetItem(Index: Integer; const Value: TRegistroM700);
begin
  Put(Index, Value);
end;

{TRegistroM800}

function TRegistroM800List.GetItem(Index: Integer): TRegistroM800;
begin
  Result := TRegistroM800(Inherited Items[Index]);
end;

function TRegistroM800List.New: TRegistroM800;
begin
  Result := TRegistroM800.Create;
  Add(Result);
end;

procedure TRegistroM800List.SetItem(Index: Integer; const Value: TRegistroM800);
begin
  Put(Index, Value);
end;

{ TRegistroM800 }

constructor TRegistroM800.Create;
begin
  FRegistroM810 := TRegistroM810List.Create;
end;

destructor TRegistroM800.Destroy;
begin
  FRegistroM810.Free;
  inherited;
end;

{TRegistroM810}

function TRegistroM810List.GetItem(Index: Integer): TRegistroM810;
begin
  Result := TRegistroM810(Inherited Items[Index]);
end;

function TRegistroM810List.New: TRegistroM810;
begin
  Result := TRegistroM810.Create;
  Add(Result);
end;

procedure TRegistroM810List.SetItem(Index: Integer; const Value: TRegistroM810);
begin
  Put(Index, Value);
end;

{ TRegistroM200 }

constructor TRegistroM200.Create;
begin
  FRegistroM205 := TRegistroM205List.Create;
  FRegistroM210 := TRegistroM210List.Create;
end;

destructor TRegistroM200.Destroy;
begin
  FRegistroM205.Free;
  FRegistroM210.Free;
  inherited;
end;

{ TRegistroM600 }

constructor TRegistroM600.Create;
begin
  FRegistroM605 := TRegistroM605List.Create;
  FRegistroM610 := TRegistroM610List.Create;
end;

destructor TRegistroM600.Destroy;
begin
  FRegistroM610.Free;
  FRegistroM605.Free;
  inherited;
end;

end.
