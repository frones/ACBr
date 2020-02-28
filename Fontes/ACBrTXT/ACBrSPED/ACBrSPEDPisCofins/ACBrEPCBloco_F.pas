{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Paulo Junqueira               }
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

unit ACBrEPCBloco_F;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrEPCBlocos;

type
  TRegistroF010List = class;
  TRegistroF100List = class;
  TRegistroF111List = class;
  TRegistroF120List = class;
  TRegistroF129List = class;
  TRegistroF130List = class;
  TRegistroF139List = class;
  TRegistroF150List = class;
  TRegistroF200List = class;
  TRegistroF205     = class;
  TRegistroF210List = class;
  TRegistroF211List = class;
  TRegistroF500List = class;
  TRegistroF509List = class;
  TRegistroF510List = class;
  TRegistroF519List = class;
  TRegistroF525List = class;
  TRegistroF550List = class;
  TRegistroF559List = class;
  TRegistroF560List = class;
  TRegistroF569List = class;
  TRegistroF600List = class;
  TRegistroF700List = class;
  TRegistroF800List = class;

  //REGISTRO F001: ABERTURA DO BLOCO C
  TRegistroF001 = class(TOpenBlocos)
  private
    FRegistroF010: TRegistroF010List;
  public
    constructor Create;  virtual;  /// Create
    destructor  Destroy; override; /// Destroy

    property RegistroF010: TRegistroF010List read FRegistroF010 write FRegistroF010;
  end;

  //REGISTRO F010: IDENTIFICAÇÃO DO ESTABELECIMENTO
  TRegistroF010 = class
  private
    fCNPJ                 : string;           //02	CNPJ	Número de inscrição do estabelecimento no CNPJ.	N	014*	-

    FRegistroF100         : TRegistroF100List;
    FRegistroF120         : TRegistroF120List;
    FRegistroF130         : TRegistroF130List;
    FRegistroF150         : TRegistroF150List;
    FRegistroF200         : TRegistroF200List;
    FRegistroF500         : TRegistroF500List;
    FRegistroF510         : TRegistroF510List;
    FRegistroF525         : TRegistroF525List;
    FRegistroF550         : TRegistroF550List;
    FRegistroF560         : TRegistroF560List;
    FRegistroF600         : TRegistroF600List;
    FRegistroF700         : TRegistroF700List;
    FRegistroF800         : TRegistroF800List;
  public
    constructor Create; virtual;              /// Create
    destructor Destroy; override;             /// Destroy

    property CNPJ         : string            read FCNPJ         write FCNPJ;

    property RegistroF100 : TRegistroF100List read FRegistroF100 write FRegistroF100;
    property RegistroF120 : TRegistroF120List read FRegistroF120 write FRegistroF120;
    property RegistroF130 : TRegistroF130List read FRegistroF130 write FRegistroF130;
    property RegistroF150 : TRegistroF150List read FRegistroF150 write FRegistroF150;
    property RegistroF200 : TRegistroF200List read FRegistroF200 write FRegistroF200;
    property RegistroF500 : TRegistroF500List read FRegistroF500 write FRegistroF500;
    property RegistroF510 : TRegistroF510List read FRegistroF510 write FRegistroF510;
    property RegistroF525 : TRegistroF525List read FRegistroF525 write FRegistroF525;
    property RegistroF550 : TRegistroF550List read FRegistroF550 write FRegistroF550;
    property RegistroF560 : TRegistroF560List read FRegistroF560 write FRegistroF560;
    property RegistroF600 : TRegistroF600List read FRegistroF600 write FRegistroF600;
    property RegistroF700 : TRegistroF700List read FRegistroF700 write FRegistroF700;
    property RegistroF800 : TRegistroF800List read FRegistroF800 write FRegistroF800;
  end;

  // Registro F010 - Lista
  TRegistroF010List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF010;
    procedure SetItem(Index: Integer; const Value: TRegistroF010);
  public
    function New: TRegistroF010;
    property Items[Index: Integer]: TRegistroF010 read GetItem write SetItem;
  end;

  //REGISTRO F100: COMPLEMENTO DO DOCUMENTO - INFORMAÇÃO COMPLEMENTAR DA NOTA FISCAL
  TRegistroF100 = class
  private
    FIND_OPER              : TACBrIndTpOperacaoReceita;
    FCOD_PART              : string;                  //Código do participante (Campo 02 do Registro 0150)
    FCOD_ITEM              : string;                  //Código do item (campo 02 do Registro 0200)
    FDT_OPER               : TDateTime;
    FVL_OPER               : currency;                //Valor da Operação/Item
    FCST_PIS               : TACBrCstPis;             //Código da Situação Tributária referente ao PIS/PASEP, conforme a Tabela indicada no item 4.3.3.
    FVL_BC_PIS             : currency;                //Valor da Base de cálculo do PIS/PASEP
    FALIQ_PIS              : currency;                //Alíquota do PIS/PASEP (em percentual)
    FVL_PIS                : currency;                //Valor do PIS/PASEP
    FCST_COFINS            : TACBrSituacaoTribCOFINS; //Código da Situação Tributária referente a COFINS, conforme a Tabela indicada no item 4.3.4.
    FVL_BC_COFINS          : currency;                //Valor da Base de cálculo da COFINS
    FALIQ_COFINS           : currency;                //Alíquota da COFINS (em percentual)
    FVL_COFINS             : currency;                //Valor da COFINS
    FNAT_BC_CRED           : TACBrBaseCalculoCredito; //Código da Base de Cálculo dos Créditos, conforme a tabela indicada no item 4.3.7, caso seja informado código representativo de crédito nos Campos 07 (CST_PIS) e 11 (CST_COFINS).
    FIND_ORIG_CRED         : TACBrOrigemCredito;      //Indicador da origem do crédito: 0 - Operação no Mercado Interno 1 - Operação de Importação
    FCOD_CTA               : string;                  //Código da conta analítica contábil debitada/creditada
    FCOD_CCUS              : string;                  //Código do Centro de Custos
    FDESC_DOC_OPER         : string;                  //Descrição  do Documento/Operação

    FRegistroF111          : TRegistroF111List;
  public
    constructor Create;  virtual;                     /// Create
    destructor  Destroy; override;                    /// Destroy

    property IND_OPER      : TACBrIndTpOperacaoReceita read FIND_OPER      write FIND_OPER;
    property COD_PART      : string                   read FCOD_PART      write FCOD_PART;
    property COD_ITEM      : string                   read FCOD_ITEM      write FCOD_ITEM;
    property DT_OPER       : TDateTime                read FDT_OPER       write FDT_OPER;
    property VL_OPER       : currency                 read FVL_OPER       write FVL_OPER;
    property CST_PIS       : TACBrCstPis              read FCST_PIS       write FCST_PIS;
    property VL_BC_PIS     : currency                 read FVL_BC_PIS     write FVL_BC_PIS;
    property ALIQ_PIS      : currency                 read FALIQ_PIS      write FALIQ_PIS;
    property VL_PIS        : currency                 read FVL_PIS        write FVL_PIS;
    property CST_COFINS    : TACBrSituacaoTribCOFINS  read FCST_COFINS    write FCST_COFINS;
    property VL_BC_COFINS  : currency                 read FVL_BC_COFINS  write FVL_BC_COFINS;
    property ALIQ_COFINS   : currency                 read FALIQ_COFINS   write FALIQ_COFINS;
    property VL_COFINS     : currency                 read FVL_COFINS     write FVL_COFINS;
    property NAT_BC_CRED   : TACBrBaseCalculoCredito  read FNAT_BC_CRED   write FNAT_BC_CRED;
    property IND_ORIG_CRED : TACBrOrigemCredito       read FIND_ORIG_CRED write FIND_ORIG_CRED;
    property COD_CTA       : string                   read FCOD_CTA       write FCOD_CTA;
    property COD_CCUS      : string                   read FCOD_CCUS      write FCOD_CCUS;
    property DESC_DOC_OPER : string                   read FDESC_DOC_OPER write FDESC_DOC_OPER;

    property RegistroF111  : TRegistroF111List        read FRegistroF111  write FRegistroF111;
  end;

  // Registro F100 - Lista
  TRegistroF100List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF100;
    procedure SetItem(Index: Integer; const Value: TRegistroF100);
  public
    function New: TRegistroF100;
    property Items[Index: Integer]: TRegistroF100 read GetItem write SetItem;
  end;

  //REGISTRO F111: PROCESSO REFERENCIADO
  TRegistroF111 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro F111 - Lista
  TRegistroF111List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF111;
    procedure SetItem(Index: Integer; const Value: TRegistroF111);
  public
    function New: TRegistroF111;
    property Items[Index: Integer]: TRegistroF111 read GetItem write SetItem;
  end;

  //REGISTRO F120: BENS INCORPORADOS AO ATIVO IMOBILIZADO - OPERAÇÕES GERADORAS DE CRÉDITOS COM BASE NOS ENCARGOS DE DEPRECIAÇÀO E AMORTIZAÇÃO
  TRegistroF120 = class
  private
    fNAT_BC_CRED                   : TACBrBaseCalculoCredito;          //Código da Base de Cálculo do Crédito sobre Bens Incorporados ao Ativo Imobilizado, conforme a Tabela indicada no item 4.3.7: 09 = Crédito com Base nos Encargos de Depreciação; 11 = Crédito com Base nos Encargos de Amortização
    fIDENT_BEM_IMOB                : string;                           //Identificação dos Bens/Grupo de Bens Incorporados ao Ativo Imobilizado:
                                                                         //01 = Edificações e Benfeitorias em Imóveis Próprios;
                                                                         //02 = Edificações e Benfeitorias em Imóveis de Terceiros;
                                                                         //03 = Instalações;
                                                                         //04 = Máquinas;
                                                                         //05 = Equipamentos;
                                                                         //06 = Veículos;
                                                                         //99 = Outros Bens Incorporados ao Ativo Imobilizado.
    fIND_ORIG_CRED                 : string;                           //Indicador da origem do bem incorporado ao ativo imobilizado, gerador de crédito: 0 - Aquisição no Mercado Interno 1 - Aquisição no Mercado Externo (Importação)
    fIND_UTIL_BEM_IMOB             : string;                           //Indicador da Utilização dos Bens Incorporados ao Ativo Imobilizado:
                                                                         //1 - Produção de Bens Destinados a Venda;
                                                                         //2 - Prestação de Serviços;
                                                                         //3 - Locação a Terceiros;
                                                                         //9 - Outros.
    fVL_OPER_DEP                   : currency;                         //Valor do Encargo de Depreciação/Amortização Incorrido no Período
    fPARC_OPER_NAO_BC_CRED         : currency;                         //Parcela do Valor do Encargo de Depreciação/Amortização a excluir da base de cálculo de Crédito
    fCST_PIS                       : TACBrCstPis;                      //Código da Situação Tributária referente ao PIS/PASEP, conforme a Tabela indicada no item 4.3.3.
    fVL_BC_PIS                     : Currency;                         //Base de cálculo do Crédito de PIS/PASEP no período
    fALIQ_PIS                      : currency;                         //Alíquota do PIS/PASEP (em percentual)
    fVL_PIS                        : currency;                         //Valor do Crédito de PIS/PASEP
    fCST_COFINS                    : TACBrSituacaoTribCOFINS;          //Código da Situação Tributária referente a COFINS, conforme a Tabela indicada no item 4.3.4.
    fVL_BC_COFINS                  : currency;                         //Base de Cálculo do Crédito da COFINS no período (06 - 07)
    fALIQ_COFINS                   : currency;                         //Alíquota da COFINS (em percentual)
    fVL_COFINS                     : currency;                         //Valor do crédito da COFINS
    fCOD_CTA                       : string;                           //Código da conta analítica contábil debitada/creditada
    fCOD_CCUS                      : string;                           //Código do Centro de Custos
    fDESC_BEM_IMOB                 : string;                           //Descrição complementar do bem ou grupo de bens, com crédito apurado com base nos encargos de depreciação ou amortização.

    FRegistroF129                  : TRegistroF129List;
  public
    constructor Create; virtual;                                       /// Create
    destructor Destroy; override;                                      /// Destroy

    property NAT_BC_CRED           : TACBrBaseCalculoCredito read FNAT_BC_CRED           write FNAT_BC_CRED;
    property IDENT_BEM_IMOB        : string                  read fIDENT_BEM_IMOB        write fIDENT_BEM_IMOB;
    property IND_ORIG_CRED         : string                  read fIND_ORIG_CRED         write fIND_ORIG_CRED;
    property IND_UTIL_BEM_IMOB     : string                  read fIND_UTIL_BEM_IMOB     write fIND_UTIL_BEM_IMOB;
    property VL_OPER_DEP           : currency                read fVL_OPER_DEP           write fVL_OPER_DEP;
    property PARC_OPER_NAO_BC_CRED : currency                read fPARC_OPER_NAO_BC_CRED write fPARC_OPER_NAO_BC_CRED;
    property CST_PIS               : TACBrCstPis             read fCST_PIS               write fCST_PIS;
    property VL_BC_PIS             : currency                read fVL_BC_PIS             write fVL_BC_PIS;
    property ALIQ_PIS              : currency                read fALIQ_PIS              write fALIQ_PIS;
    property VL_PIS                : currency                read fVL_PIS                write fVL_PIS;
    property CST_COFINS            : TACBrSituacaoTribCOFINS read fCST_COFINS            write fCST_COFINS;
    property VL_BC_COFINS          : currency                read fVL_BC_COFINS          write fVL_BC_COFINS;
    property ALIQ_COFINS           : currency                read fALIQ_COFINS           write fALIQ_COFINS;
    property VL_COFINS             : currency                read fVL_COFINS             write fVL_COFINS;
    property COD_CTA               : string                  read fCOD_CTA               write fCOD_CTA;
    property COD_CCUS              : string                  read fCOD_CCUS              write fCOD_CCUS;
    property DESC_BEM_IMOB         : string                  read fDESC_BEM_IMOB         write fDESC_BEM_IMOB;

    property RegistroF129          : TRegistroF129List       read FRegistroF129          write FRegistroF129;
  end;

  // Registro F120 - Lista
  TRegistroF120List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF120;
    procedure SetItem(Index: Integer; const Value: TRegistroF120);
  public
    function New: TRegistroF120;
    property Items[Index: Integer]: TRegistroF120 read GetItem write SetItem;
  end;

  //REGISTRO F129: PROCESSO REFERENCIADO
  TRegistroF129 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro F129 - Lista
  TRegistroF129List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF129;
    procedure SetItem(Index: Integer; const Value: TRegistroF129);
  public
    function New: TRegistroF129;
    property Items[Index: Integer]: TRegistroF129 read GetItem write SetItem;
  end;

  //REGISTRO F130: BENS INCORPORADOS AO ATIVO IMOBILIZADO - OPERAÇÕES GERADORAS DE CRÉDITOS COM BASE NO VALOR DE AQUISIÇÃO/CONTRIBUIÇÃO
  TRegistroF130 = class
  private
    fNAT_BC_CRED                   : TACBrBaseCalculoCredito;          //Código da Base de Cálculo do Crédito sobre Bens Incorporados ao Ativo Imobilizado, conforme a Tabela indicada no item 4.3.7: 09 = Crédito com Base nos Encargos de Depreciação; 11 = Crédito com Base nos Encargos de Amortização
    fIDENT_BEM_IMOB                : string;                           //Identificação dos Bens/Grupo de Bens Incorporados ao Ativo Imobilizado:
                                                                         //01 = Edificações e Benfeitorias em Imóveis Próprios;
                                                                         //02 = Edificações e Benfeitorias em Imóveis de Terceiros;
                                                                         //03 = Instalações;
                                                                         //04 = Máquinas;
                                                                         //05 = Equipamentos;
                                                                         //06 = Veículos;
                                                                         //99 = Outros Bens Incorporados ao Ativo Imobilizado.
    fIND_ORIG_CRED                 : string;                           //Indicador da origem do bem incorporado ao ativo imobilizado, gerador de crédito: 0 - Aquisição no Mercado Interno 1 - Aquisição no Mercado Externo (Importação)
    fIND_UTIL_BEM_IMOB             : string;                           //Indicador da Utilização dos Bens Incorporados ao Ativo Imobilizado:
                                                                         //1 - Produção de Bens Destinados a Venda;
                                                                         //2 - Prestação de Serviços;
                                                                         //3 - Locação a Terceiros;
                                                                         //9 - Outros.
    fMES_OPER_AQUIS                : string;                           //Mês/Ano de Aquisição dos Bens Incorporados ao Ativo Imobilizado, com apuração de crédito com base no valor de aquisição.
    fVL_OPER_AQUIS                 : currency;                         //Valor de Aquisição dos Bens Incorporados ao Ativo Imobilizado - Crédito com base no valor de aquisição.
    fPARC_OPER_NAO_BC_CRED         : currency;                         //Parcela do Valor de Aquisição a excluir da base de cálculo de Crédito
    fVL_BC_CRED                    : currency;                         //Valor da Base de Cálculo do Crédito sobre Bens Incorporados ao Ativo Imobilizado (07 - 08)
    fIND_NR_PARC                   : integer;                          //Indicador do Numero de Parcelas a serem apropriadas (Crédito sobre Valor de Aquisição):
                                                                         //1 - Integral (Mês de Aquisição);
                                                                         //2 - 12 Meses;
                                                                         //3 - 24 Meses;
                                                                         //4 - 48 Meses;
                                                                         //5 - 6 Meses (Embalagens de bebidas frias)
                                                                         //9 - Outra periodicidade definida em Lei.
    fCST_PIS                       : TACBrCstPis;                      //Código da Situação Tributária referente ao PIS/PASEP, conforme a Tabela indicada no item 4.3.3.
    fVL_BC_PIS                     : currency;                         //Base de cálculo Mensal do Crédito de PIS/PASEP, conforme indicador informado no campo 10.
    fALIQ_PIS                      : currency;                         //Alíquota do PIS/PASEP
    fVL_PIS                        : currency;                         //Valor do Crédito de PIS/PASEP
    fCST_COFINS                    : TACBrSituacaoTribCOFINS;          //Código da Situação Tributária referente a COFINS, conforme a Tabela indicada no item 4.3.4.
    fVL_BC_COFINS                  : currency;                         //Base de Cálculo Mensal do Crédito da COFINS, conforme indicador informado no campo 10.
    fALIQ_COFINS                   : currency;                         //Alíquota da COFINS
    fVL_COFINS                     : currency;                         //Valor do crédito da COFINS
    fCOD_CTA                       : string;                           //Código da conta analítica contábil debitada/creditada
    fCOD_CCUS                      : string;                           //Código do Centro de Custos
    fDESC_BEM_IMOB                 : string;                           //Descrição complementar do bem ou grupo de bens, com crédito apurado com base no valor de aquisição.

    FRegistroF139                  : TRegistroF139List;
  public
    constructor Create;  virtual;                                      /// Create
    destructor  Destroy; override;                                     /// Destroy

    property NAT_BC_CRED           : TACBrBaseCalculoCredito read FNAT_BC_CRED           write FNAT_BC_CRED;
    property IDENT_BEM_IMOB        : string                  read fIDENT_BEM_IMOB        write fIDENT_BEM_IMOB;
    property IND_ORIG_CRED         : string                  read fIND_ORIG_CRED         write fIND_ORIG_CRED;
    property IND_UTIL_BEM_IMOB     : string                  read fIND_UTIL_BEM_IMOB     write fIND_UTIL_BEM_IMOB;
    property MES_OPER_AQUIS        : string                  read fMES_OPER_AQUIS        write fMES_OPER_AQUIS;
    property VL_OPER_AQUIS         : currency                read fVL_OPER_AQUIS         write fVL_OPER_AQUIS;
    property PARC_OPER_NAO_BC_CRED : currency                read fPARC_OPER_NAO_BC_CRED write fPARC_OPER_NAO_BC_CRED;
    property VL_BC_CRED            : currency                read fVL_BC_CRED            write fVL_BC_CRED;
    property IND_NR_PARC           : integer                 read fIND_NR_PARC           write fIND_NR_PARC;
    property CST_PIS               : TACBrCstPis             read fCST_PIS               write fCST_PIS;
    property VL_BC_PIS             : currency                read fVL_BC_PIS             write fVL_BC_PIS;
    property ALIQ_PIS              : currency                read fALIQ_PIS              write fALIQ_PIS;
    property VL_PIS                : currency                read fVL_PIS                write fVL_PIS;
    property CST_COFINS            : TACBrSituacaoTribCOFINS read fCST_COFINS            write fCST_COFINS;
    property VL_BC_COFINS          : currency                read fVL_BC_COFINS          write fVL_BC_COFINS;
    property ALIQ_COFINS           : currency                read fALIQ_COFINS           write fALIQ_COFINS;
    property VL_COFINS             : currency                read fVL_COFINS             write fVL_COFINS;
    property COD_CTA               : string                  read fCOD_CTA               write fCOD_CTA;
    property COD_CCUS              : string                  read fCOD_CCUS              write fCOD_CCUS;
    property DESC_BEM_IMOB         : string                  read fDESC_BEM_IMOB         write fDESC_BEM_IMOB;

    property RegistroF139          : TRegistroF139List       read FRegistroF139          write FRegistroF139;
  end;

  // Registro F130 - Lista
  TRegistroF130List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF130;
    procedure SetItem(Index: Integer; const Value: TRegistroF130);
  public
    function New: TRegistroF130;
    property Items[Index: Integer]: TRegistroF130 read GetItem write SetItem;
  end;

  //REGISTRO F139: PROCESSO REFERENCIADO
  TRegistroF139 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro F139 - Lista
  TRegistroF139List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF139;
    procedure SetItem(Index: Integer; const Value: TRegistroF139);
  public
    function New: TRegistroF139;
    property Items[Index: Integer]: TRegistroF139 read GetItem write SetItem;
  end;

  //REGISTRO F150: CRÉDITO PRESUMIDO SOBRE ESTOQUE DE ABERTURA
  TRegistroF150 = class
  private
    FNAT_BC_CRED            : TACBrBaseCalculoCredito;          //Texto fixo contendo "18" Código da Base de Cálculo do Crédito sobre Estoque de Abertura, conforme a Tabela indicada no item 4.3.7.
    FVL_TOT_EST             : currency;                         //Valor Total do Estoque de Abertura
    FEST_IMP                : integer;                          //Parcela do estoque de abertura referente a bens, produtos e mercadorias importados, ou adquiridas no mercado interno sem direito ao crédito
    FVL_BC_EST              : currency;                         //Valor da Base de Cálculo do Crédito sobre o Estoque de Abertura (03 - 04)
    FVL_BC_MEN_EST          : currency;                         //Valor da Base de Cálculo Mensal do Crédito sobre o Estoque de Abertura (1/12 avos do campo 05)
    FCST_PIS                : TACBrCstPis;                      //Código da Situação Tributária referente ao PIS/PASEP, conforme a Tabela indicada no item 4.3.3.
    FALIQ_PIS               : currency;                         //Alíquota do PIS/PASEP
    FVL_CRED_PIS            : currency;                         //Valor Mensal do Crédito Presumido Apurado para o Período -  PIS/PASEP  (06 x 08)
    FALIQ_COFINS            : currency;                         //Código da Situação Tributária referente a COFINS, conforme a Tabela indicada no item 4.3.4.
    FVL_CRED_COFINS         : currency;                         //Alíquota da COFINS
    FCOD_CTA                : string;                           //Valor Mensal do Crédito Presumido Apurado para o Período -  COFINS (06 x 11)
    FCST_COFINS             : TACBrSituacaoTribCOFINS;          //Descrição do estoque
    FDESC_EST               : string;                           //Código da conta analítica contábil debitada/creditada
  public
    property NAT_BC_CRED    : TACBrBaseCalculoCredito read FNAT_BC_CRED    write FNAT_BC_CRED;
    property VL_TOT_EST     : currency                read FVL_TOT_EST     write FVL_TOT_EST;
    property EST_IMP        : integer                 read FEST_IMP        write FEST_IMP;
    property VL_BC_EST      : currency                read FVL_BC_EST      write FVL_BC_EST;
    property VL_BC_MEN_EST  : currency                read FVL_BC_MEN_EST  write FVL_BC_MEN_EST;
    property CST_PIS        : TACBrCstPis             read FCST_PIS        write FCST_PIS;
    property ALIQ_PIS       : currency                read FALIQ_PIS       write FALIQ_PIS;
    property VL_CRED_PIS    : currency                read FVL_CRED_PIS    write FVL_CRED_PIS;
    property CST_COFINS     : TACBrSituacaoTribCOFINS read FCST_COFINS     write FCST_COFINS;
    property ALIQ_COFINS    : currency                read FALIQ_COFINS    write FALIQ_COFINS;
    property VL_CRED_COFINS : currency                read FVL_CRED_COFINS write FVL_CRED_COFINS;
    property DESC_EST       : string                  read FDESC_EST       write FDESC_EST;
    property COD_CTA        : string                  read FCOD_CTA        write FCOD_CTA;
  end;

  // Registro F150 - Lista
  TRegistroF150List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF150;
    procedure SetItem(Index: Integer; const Value: TRegistroF150);
  public
    function New: TRegistroF150;
    property Items[Index: Integer]: TRegistroF150 read GetItem write SetItem;
  end;

  //REGISTRO F200: OPERAÇÕES DA ATIVIDADE IMOBILIÁRIA - UNIDADE IMOBILIÁRIA VENDIDA
  TRegistroF200 = class
  private
    FALIQ_PIS               : currency;
    FVL_REC_ACUM            : currency;
    FVL_BC_COFINS           : currency;
    FVL_BC_PIS              : currency;
    FALIQ_COFINS            : currency;
    FVL_TOT_VEND            : currency;
    FPERC_REC_RECEB         : currency;
    FVL_TOT_REC             : currency;
    FVL_COFINS              : currency;
    FVL_PIS                 : currency;
    FCST_COFINS             : TACBrSituacaoTribCOFINS;
    FCPF_CNPJ_ADQU          : string;
    FUNID_IMOB              : string;
    FCST_PIS                : TACBrCstPis;
    FIDENT_EMP              : string;
    FNUM_CONT               : string;
    FIND_OPER               : string;
    FINF_COMP               : string;
    FDESC_UNID_IMOB         : string;
    FIND_NAT_EMP            : string;
    FDT_OPER                : TDateTime;

    FRegistroF205           : TRegistroF205;
    FRegistroF210           : TRegistroF210List;
    FRegistroF211           : TRegistroF211List;
  public
    constructor Create; virtual;  /// Create
    destructor Destroy; override; /// Destroy

    property IND_OPER       : string                   read FIND_OPER       write FIND_OPER;
    property UNID_IMOB      : string                   read FUNID_IMOB      write FUNID_IMOB;
    property IDENT_EMP      : string                   read FIDENT_EMP      write FIDENT_EMP;
    property DESC_UNID_IMOB : string                   read FDESC_UNID_IMOB write FDESC_UNID_IMOB;
    property NUM_CONT       : string                   read FNUM_CONT       write FNUM_CONT;
    property CPF_CNPJ_ADQU  : string                   read FCPF_CNPJ_ADQU  write FCPF_CNPJ_ADQU;
    property DT_OPER        : TDateTime                read FDT_OPER        write FDT_OPER;
    property VL_TOT_VEND    : currency                 read FVL_TOT_VEND    write FVL_TOT_VEND;
    property VL_REC_ACUM    : currency                 read FVL_REC_ACUM    write FVL_REC_ACUM;
    property VL_TOT_REC     : currency                 read FVL_TOT_REC     write FVL_TOT_REC;
    property CST_PIS        : TACBrCstPis              read FCST_PIS        write FCST_PIS;
    property VL_BC_PIS      : currency                 read FVL_BC_PIS      write FVL_BC_PIS;
    property ALIQ_PIS       : currency                 read FALIQ_PIS       write FALIQ_PIS;
    property VL_PIS         : currency                 read FVL_PIS         write FVL_PIS;
    property CST_COFINS     : TACBrSituacaoTribCOFINS  read FCST_COFINS     write FCST_COFINS;
    property VL_BC_COFINS   : currency                 read FVL_BC_COFINS   write FVL_BC_COFINS;
    property ALIQ_COFINS    : currency                 read FALIQ_COFINS    write FALIQ_COFINS;
    property VL_COFINS      : currency                 read FVL_COFINS      write FVL_COFINS;
    property PERC_REC_RECEB : currency                 read FPERC_REC_RECEB write FPERC_REC_RECEB;
    property IND_NAT_EMP    : string                   read FIND_NAT_EMP    write FIND_NAT_EMP;
    property INF_COMP       : string                   read FINF_COMP       write FINF_COMP;

    property RegistroF205   : TRegistroF205            read FRegistroF205   write FRegistroF205;
    property RegistroF210   : TRegistroF210List        read FRegistroF210   write FRegistroF210;
    property RegistroF211   : TRegistroF211List        read FRegistroF211   write FRegistroF211;
  end;

  // Registro F200 - Lista
  TRegistroF200List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF200;
    procedure SetItem(Index: Integer; const Value: TRegistroF200);
  public
    function New: TRegistroF200;
    property Items[Index: Integer]: TRegistroF200 read GetItem write SetItem;
  end;

  //REGISTRO F205: OPERAÇÕES DA ATIVIDADE IMOBILIÁRIA - CUSTO INCORRIDO DA UNIDADE IMOBILIÁRIA
  TRegistroF205 = class
  private
    FVL_CRED_PIS_ACUM                : currency;
    FVL_EXC_BC_CUS_INC_ACUM          : currency;
    FVL_CUS_INC_PER_ESC              : currency;
    FVL_CRED_COFINS_DESC_FUT         : currency;
    FVL_BC_CUS_INC                   : currency;
    FALIQ_COFINS                     : currency;
    FVL_CRED_PIS_DESC_FUT            : currency;
    FVL_CRED_COFINS_DESC_ANT         : currency;
    FVL_CRED_PIS_DESC_ANT            : currency;
    FVL_CRED_COFINS_ACUM             : currency;
    FVL_CRED_PIS_DESC                : currency;
    FVL_CUS_INC_ACUM_ANT             : currency;
    FVL_CRED_COFINS_DESC             : currency;
    FVL_CUS_INC_ACUM                 : currency;
    FCST_PIS                         : TACBrCstPis;
    FCST_COFINS                      : TACBrSituacaoTribCOFINS;
    FALIQ_PIS                        : Currency;
  public
    property VL_CUS_INC_ACUM_ANT     : currency                read FVL_CUS_INC_ACUM_ANT     write FVL_CUS_INC_ACUM_ANT;
    property VL_CUS_INC_PER_ESC      : currency                read FVL_CUS_INC_PER_ESC      write FVL_CUS_INC_PER_ESC;
    property VL_CUS_INC_ACUM         : currency                read FVL_CUS_INC_ACUM         write FVL_CUS_INC_ACUM;
    property VL_EXC_BC_CUS_INC_ACUM  : currency                read FVL_EXC_BC_CUS_INC_ACUM  write FVL_EXC_BC_CUS_INC_ACUM;
    property VL_BC_CUS_INC           : currency                read FVL_BC_CUS_INC           write FVL_BC_CUS_INC;
    property CST_PIS                 : TACBrCstPis             read FCST_PIS                 write FCST_PIS;
    property ALIQ_PIS                : currency                read FALIQ_PIS                write FALIQ_PIS;
    property VL_CRED_PIS_ACUM        : currency                read FVL_CRED_PIS_ACUM        write FVL_CRED_PIS_ACUM;
    property VL_CRED_PIS_DESC_ANT    : currency                read FVL_CRED_PIS_DESC_ANT    write FVL_CRED_PIS_DESC_ANT;
    property VL_CRED_PIS_DESC        : currency                read FVL_CRED_PIS_DESC        write FVL_CRED_PIS_DESC;
    property VL_CRED_PIS_DESC_FUT    : currency                read FVL_CRED_PIS_DESC_FUT    write FVL_CRED_PIS_DESC_FUT;
    property CST_COFINS              : TACBrSituacaoTribCOFINS read FCST_COFINS              write FCST_COFINS;
    property ALIQ_COFINS             : currency                read FALIQ_COFINS             write FALIQ_COFINS;
    property VL_CRED_COFINS_ACUM     : currency                read FVL_CRED_COFINS_ACUM     write FVL_CRED_COFINS_ACUM;
    property VL_CRED_COFINS_DESC_ANT : currency                read FVL_CRED_COFINS_DESC_ANT write FVL_CRED_COFINS_DESC_ANT;
    property VL_CRED_COFINS_DESC     : currency                read FVL_CRED_COFINS_DESC     write FVL_CRED_COFINS_DESC;
    property VL_CRED_COFINS_DESC_FUT : currency                read FVL_CRED_COFINS_DESC_FUT write FVL_CRED_COFINS_DESC_FUT;
  end;

  //REGISTRO F210: REGISTRO F210: OPERAÇÕES DA ATIVIDADE IMOBILIÁRIA - CUSTO ORÇADO DA UNIDADE IMOBILIÁRIA VENDIDA
  TRegistroF210 = class
  private
    FVL_BC_CRED                  : currency;
    FVL_CRED_COFINS_UTIL         : currency;
    FALIQ_PIS                    : currency;
    FVL_EXC                      : currency;
    FALIQ_COFINS                 : currency;
    FVL_CUS_ORC_AJU              : currency;
    FVL_CUS_ORC                  : currency;
    FVL_CRED_PIS_UTIL            : currency;
    FCST_PIS                     : TACBrCstPis;
    FCST_COFINS                  : TACBrSituacaoTribCOFINS;
  public
    property VL_CUS_ORC          : currency                read FVL_CUS_ORC          write FVL_CUS_ORC;
    property VL_EXC              : currency                read FVL_EXC              write FVL_EXC;
    property VL_CUS_ORC_AJU      : currency                read FVL_CUS_ORC_AJU      write FVL_CUS_ORC_AJU;
    property VL_BC_CRED          : currency                read FVL_BC_CRED          write FVL_BC_CRED;
    property CST_PIS             : TACBrCstPis             read FCST_PIS             write FCST_PIS;
    property ALIQ_PIS            : currency                read FALIQ_PIS            write FALIQ_PIS;
    property VL_CRED_PIS_UTIL    : currency                read FVL_CRED_PIS_UTIL    write FVL_CRED_PIS_UTIL;
    property CST_COFINS          : TACBrSituacaoTribCOFINS read FCST_COFINS          write FCST_COFINS;
    property ALIQ_COFINS         : currency                read FALIQ_COFINS         write FALIQ_COFINS;
    property VL_CRED_COFINS_UTIL : currency                read FVL_CRED_COFINS_UTIL write FVL_CRED_COFINS_UTIL;
  end;

  // Registro F210 - Lista
  TRegistroF210List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF210;
    procedure SetItem(Index: Integer; const Value: TRegistroF210);
  public
    function New: TRegistroF210;
    property Items[Index: Integer]: TRegistroF210 read GetItem write SetItem;
  end;

  //REGISTRO F211: PROCESSO REFERENCIADO
  TRegistroF211 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  // Registro F211 - Lista
  TRegistroF211List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF211;
    procedure SetItem(Index: Integer; const Value: TRegistroF211);
  public
    function New: TRegistroF211;
    property Items[Index: Integer]: TRegistroF211 read GetItem write SetItem;
  end;

  (*Edilson Alves de oliveira (22/11/2011)
    Registro F500 - Consolidação das Operações da Pessoa Jurídica Submetida ao
                    Regime de Tributação com Base no Lucro Presumido  –
                    Incidência do PIS/Pasep e da Cofins pelo Regime de Caixa*)
  TRegistroF500 = class
  private
     FVL_REC_CAIXA            : currency;
     FCST_PIS                 : TACBrCstPis;
     FVL_DESC_PIS             : currency;
     FVL_BC_PIS               : currency;
     FALIQ_PIS                : currency;
     FVL_PIS                  : currency;
     FCST_COFINS              : TACBrSituacaoTribCOFINS;
     FVL_DESC_COFINS          : currency;
     FVL_BC_COFINS            : currency;
     FALIQ_COFINS             : currency;
     FVL_COFINS               : currency;
     FCOD_MOD                 : string;
     FCFOP                    : Integer;
     FCOD_CTA                 : string;
     FINFO_COMPL              : string;
     FRegistroF509            : TRegistroF509List;

  public
     constructor Create;  virtual;                     /// Create
     destructor  Destroy; override;                    /// Destroy

     property VL_REC_CAIXA      : currency                read  FVL_REC_CAIXA   write FVL_REC_CAIXA  ;
     property CST_PIS           : TACBrCstPis             read  FCST_PIS        write FCST_PIS       ;
     property VL_DESC_PIS       : currency                read  FVL_DESC_PIS    write FVL_DESC_PIS   ;
     property VL_BC_PIS         : currency                read  FVL_BC_PIS      write FVL_BC_PIS     ;
     property ALIQ_PIS          : currency                read  FALIQ_PIS       write FALIQ_PIS      ;
     property VL_PIS            : currency                read  FVL_PIS         write FVL_PIS        ;
     property CST_COFINS        : TACBrSituacaoTribCOFINS read  FCST_COFINS     write FCST_COFINS    ;
     property VL_DESC_COFINS    : currency                read  FVL_DESC_COFINS write FVL_DESC_COFINS;
     property VL_BC_COFINS      : currency                read  FVL_BC_COFINS   write FVL_BC_COFINS  ;
     property ALIQ_COFINS       : currency                read  FALIQ_COFINS    write FALIQ_COFINS   ;
     property VL_COFINS         : currency                read  FVL_COFINS      write FVL_COFINS     ;
     property COD_MOD           : string                  read  FCOD_MOD        write FCOD_MOD       ;
     property CFOP              : Integer                 read  FCFOP           write FCFOP          ;
     property COD_CTA           : string                  read  FCOD_CTA        write FCOD_CTA       ;
     property INFO_COMPL        : string                  read  FINFO_COMPL     write FINFO_COMPL    ;

     property RegistroF509      : TRegistroF509List       read  FRegistroF509   write FRegistroF509  ;
  end;

  // Registro F500 - Lista
  TRegistroF500List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF500;
    procedure SetItem(Index: Integer; const Value: TRegistroF500);
  public
    function New: TRegistroF500;
    property Items[Index: Integer]: TRegistroF500 read GetItem write SetItem;
  end;

  //REGISTRO F509: PROCESSO REFERENCIADO
  TRegistroF509 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  TRegistroF509List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF509;
    procedure SetItem(Index: Integer; const Value: TRegistroF509);
  public
    function New: TRegistroF509;
    property Items[Index: Integer]: TRegistroF509 read GetItem write SetItem;
  end;



  (*Edilson Alves de oliveira (28/11/2011)
    Registro F510 - CONSOLIDAÇÃO  DAS  OPERAÇÕES  DA  PESSOA  JURÍDICA
                    SUBMETIDA AO REGIME DE TRIBUTAÇÃO COM BASE NO LUCRO  PRESUMIDO  –
                    INCIDÊNCIA DO PIS/PASEP E DA COFINS PELO REGIME DE CAIXA (APURAÇÃO DA
                    CONTRIBUIÇÃO POR UNIDADE DE MEDIDA DE PRODUTO – ALÍQUOTA EM REAIS) *)
  TRegistroF510 = class
  private
     FVL_REC_CAIXA            : Currency;
     FCST_PIS                 : TACBrCstPis;
     FVL_DESC_PIS             : currency;
     FQUANT_BC_PIS            : currency;
     FALIQ_PIS_QUANT          : currency;
     FVL_PIS                  : currency;
     FCST_COFINS              : TACBrSituacaoTribCOFINS;
     FVL_DESC_COFINS          : currency;
     FQUANT_BC_COFINS         : currency;
     FALIQ_COFINS_QUANT       : currency;
     FVL_COFINS               : currency;
     FCOD_MOD                 : string;
     FCFOP                    : Integer;
     FCOD_CTA                 : string;
     FINFO_COMPL              : string;
     FRegistroF519            : TRegistroF519List;
  public
     constructor Create;  virtual;                     /// Create
     destructor  Destroy; override;                    /// Destroy

     property  VL_REC_CAIXA        : currency                read FVL_REC_CAIXA       write FVL_REC_CAIXA     ;
     property  CST_PIS             : TACBrCstPis             read FCST_PIS            write FCST_PIS          ;
     property  VL_DESC_PIS         : currency                read FVL_DESC_PIS        write FVL_DESC_PIS      ;
     property  QUANT_BC_PIS        : currency                read FQUANT_BC_PIS       write FQUANT_BC_PIS     ;
     property  ALIQ_PIS_QUANT      : currency                read FALIQ_PIS_QUANT     write FALIQ_PIS_QUANT   ;
     property  VL_PIS              : currency                read FVL_PIS             write FVL_PIS           ;
     property  CST_COFINS          : TACBrSituacaoTribCOFINS read FCST_COFINS         write FCST_COFINS       ;
     property  VL_DESC_COFINS      : currency                read FVL_DESC_COFINS     write FVL_DESC_COFINS   ;
     property  QUANT_BC_COFINS     : currency                read FQUANT_BC_COFINS    write FQUANT_BC_COFINS  ;
     property  ALIQ_COFINS_QUANT   : currency                read FALIQ_COFINS_QUANT  write FALIQ_COFINS_QUANT;
     property  VL_COFINS           : currency                read FVL_COFINS          write FVL_COFINS        ;
     property  COD_MOD             : string                  read FCOD_MOD            write FCOD_MOD          ;
     property  CFOP                : Integer                 read FCFOP               write FCFOP             ;
     property  COD_CTA             : string                  read FCOD_CTA            write FCOD_CTA          ;
     property  INFO_COMPL          : string                  read FINFO_COMPL         write FINFO_COMPL       ;

     property RegistroF519         : TRegistroF519List       read FRegistroF519       write FRegistroF519;
  end;

  // Registro F510 - Lista
  TRegistroF510List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF510;
    procedure SetItem(Index: Integer; const Value: TRegistroF510);
  public
    function New: TRegistroF510;
    property Items[Index: Integer]: TRegistroF510 read GetItem write SetItem;
  end;

 //REGISTRO F519: PROCESSO REFERENCIADO
  TRegistroF519 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  TRegistroF519List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF519;
    procedure SetItem(Index: Integer; const Value: TRegistroF519);
  public
    function New: TRegistroF519;
    property Items[Index: Integer]: TRegistroF519 read GetItem write SetItem;
  end;

  TRegistroF525 = class
  private
     FVL_REC                   : Currency;
     FIND_REC                  : TACBrIndicadorDaComposicaoDaReceitaRecebida;
     FCNPJ_CPF                 : string;
     FNUM_DOC                  : string;
     FCOD_ITEM                 : string;
     FVL_REC_DET               : Currency;
     FCST_PIS                  : TACBrCstPis;
     FCST_COFINS               : TACBrSituacaoTribCOFINS;
     FINFO_COMPL               : string;
     FCOD_CTA                  : string;
  public
     property  VL_REC       : Currency       read FVL_REC     write FVL_REC     ;
     property  IND_REC      : TACBrInd_Rec   read FIND_REC    write FIND_REC    ;
     property  CNPJ_CPF     : string         read FCNPJ_CPF   write FCNPJ_CPF   ;
     property  NUM_DOC      : string         read FNUM_DOC    write FNUM_DOC    ;
     property  COD_ITEM     : string         read FCOD_ITEM   write FCOD_ITEM   ;
     property  VL_REC_DET   : Currency       read FVL_REC_DET write FVL_REC_DET ;
     property  CST_PIS      : TACBrCstPis    read FCST_PIS    write FCST_PIS    ;
     property  CST_COFINS   : TACBrCstCofins read FCST_COFINS write FCST_COFINS ;
     property  INFO_COMPL   : string         read FINFO_COMPL write FINFO_COMPL ;
     property  COD_CTA      : string         read FCOD_CTA    write FCOD_CTA    ;
  end;

  // Registro F525 - Lista
  TRegistroF525List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF525;
    procedure SetItem(Index: Integer; const Value: TRegistroF525);
  public
    function New: TRegistroF525;
    property Items[Index: Integer]: TRegistroF525 read GetItem write SetItem;
  end;

   //REGISTRO F550: CONSOLIDAÇÃO DAS OPERAÇÕES DA PESSOA JURÍDICA SUBMETIDA AO REGIME DE TRIBUTAÇÃO COM BASE NO LUCRO PRESUMIDO –  INCIDÊNCIA DO PIS/PASEP E DA COFINS PELO REGIME DE COMPETÊNCIA
  TRegistroF550 = class
  private
    FVL_REC_COMP             : currency;
    FCST_PIS                 : TACBrCstPis;
    FVL_DESC_PIS             : currency;
    FVL_BC_PIS               : currency;
    FALIQ_PIS                : currency;
    FVL_PIS                  : currency;
    FCST_COFINS              : TACBrCstCofins;
    FVL_DESC_COFINS          : currency;
    FVL_BC_COFINS            : currency;
    FALIQ_COFINS             : currency;
    FVL_COFINS               : currency;
    FCOD_MOD                 : String;
    FCFOP                    : Integer;
    FCOD_CTA                 : String;
    FINFO_COMPL              : String;
    FRegistroF559            : TRegistroF559List;
  public
    constructor Create;  virtual;                     /// Create
    destructor  Destroy; override;                    /// Destroy

    property VL_REC_COMP     : currency          read FVL_REC_COMP          write FVL_REC_COMP;
    property CST_PIS         : TACBrCstPis       read FCST_PIS              write FCST_PIS;
    property VL_DESC_PIS     : currency          read FVL_DESC_PIS          write FVL_DESC_PIS;
    property VL_BC_PIS       : currency          read FVL_BC_PIS            write FVL_BC_PIS;
    property ALIQ_PIS        : currency          read FALIQ_PIS             write FALIQ_PIS;
    property VL_PIS          : currency          read FVL_PIS               write FVL_PIS;
    property CST_COFINS      : TACBrCstCofins    read FCST_COFINS           write FCST_COFINS;
    property VL_DESC_COFINS  : currency          read FVL_DESC_COFINS       write FVL_DESC_COFINS;
    property VL_BC_COFINS    : currency          read FVL_BC_COFINS         write FVL_BC_COFINS;
    property ALIQ_COFINS     : currency          read FALIQ_COFINS          write FALIQ_COFINS;
    property VL_COFINS       : currency          read FVL_COFINS            write FVL_COFINS;
    property COD_MOD         : String            read FCOD_MOD              write FCOD_MOD;
    property CFOP            : Integer           read FCFOP                 write FCFOP;
    property COD_CTA         : String            read FCOD_CTA              write FCOD_CTA;
    property INFO_COMPL      : String            read FINFO_COMPL           write FINFO_COMPL;

    property RegistroF559    : TRegistroF559List read FRegistroF559         write FRegistroF559;
  end;

  // Registro F550 - Lista
  TRegistroF550List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF550;
    procedure SetItem(Index: Integer; const Value: TRegistroF550);
  public
    function New: TRegistroF550;
    property Items[Index: Integer]: TRegistroF550 read GetItem write SetItem;
  end;


  //REGISTRO F559: PROCESSO REFERENCIADO
  TRegistroF559 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  TRegistroF559List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF559;
    procedure SetItem(Index: Integer; const Value: TRegistroF559);
  public
    function New: TRegistroF559;
    property Items[Index: Integer]: TRegistroF559 read GetItem write SetItem;
  end;




  (*Edilson Alves de oliveira (23/11/2011)
    Registro F560 - CONSOLIDAÇÃO  DAS  OPERAÇÕES  DA  PESSOA  JURÍDICA
                    SUBMETIDA AO REGIME DE TRIBUTAÇÃO COM BASE NO LUCRO  PRESUMIDO  –
                    INCIDÊNCIA  DO  PIS/PASEP  E  DA  COFINS  PELO  REGIME  DE  COMPETÊNCIA
                    (APURAÇÃO  DA  CONTRIBUIÇÃO  POR  UNIDADE  DE  MEDIDA  DE  PRODUTO  –
                    ALÍQUOTA EM REAIS) *)
  TRegistroF560 = class
  private
     FVL_REC_COMP          : currency;
     FCST_PIS              : TACBrCstPis;
     FVL_DESC_PIS          : currency;
     FQUANT_BC_PIS         : currency;
     FALIQ_PIS_QUANT       : currency;
     FVL_PIS               : currency;
     FCST_COFINS           : TACBrCstCofins;
     FVL_DESC_COFINS       : currency;
     FQUANT_BC_COFINS      : currency;
     FALIQ_COFINS_QUANT    : currency;
     FVL_COFINS            : currency;
     FCOD_MOD              : string;
     FCFOP                 : Integer;
     FCOD_CTA              : string;
     FINFO_COMPL           : string;
     FRegistroF569: TRegistroF569List;
  public
     constructor Create;  virtual;                     /// Create
     destructor  Destroy; override;                    /// Destroy

     property  VL_REC_COMP        : currency          read FVL_REC_COMP        write FVL_REC_COMP      ;
     property  CST_PIS            : TACBrCstPis       read FCST_PIS            write FCST_PIS          ;
     property  VL_DESC_PIS        : currency          read FVL_DESC_PIS        write FVL_DESC_PIS      ;
     property  QUANT_BC_PIS       : currency          read FQUANT_BC_PIS       write FQUANT_BC_PIS     ;
     property  ALIQ_PIS_QUANT     : currency          read FALIQ_PIS_QUANT     write FALIQ_PIS_QUANT   ;
     property  VL_PIS             : currency          read FVL_PIS             write FVL_PIS           ;
     property  CST_COFINS         : TACBrCstCofins    read FCST_COFINS         write FCST_COFINS       ;
     property  VL_DESC_COFINS     : currency          read FVL_DESC_COFINS     write FVL_DESC_COFINS   ;
     property  QUANT_BC_COFINS    : currency          read FQUANT_BC_COFINS    write FQUANT_BC_COFINS  ;
     property  ALIQ_COFINS_QUANT  : currency          read FALIQ_COFINS_QUANT  write FALIQ_COFINS_QUANT;
     property  VL_COFINS          : currency          read FVL_COFINS          write FVL_COFINS        ;
     property  COD_MOD            : string            read FCOD_MOD            write FCOD_MOD          ;
     property  CFOP               : Integer           read FCFOP               write FCFOP             ;
     property  COD_CTA            : string            read FCOD_CTA            write FCOD_CTA          ;
     property  INFO_COMPL         : string            read FINFO_COMPL         write FINFO_COMPL       ;

     property  RegistroF569       : TRegistroF569List read FRegistroF569       write FRegistroF569     ;
  end;


  // Registro F560 - Lista
  TRegistroF560List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF560;
    procedure SetItem(Index: Integer; const Value: TRegistroF560);
  public
    function New: TRegistroF560;
    property Items[Index: Integer]: TRegistroF560 read GetItem write SetItem;
  end;


 //REGISTRO F519: PROCESSO REFERENCIADO
  TRegistroF569 = class
  private
    fNUM_PROC         : string;              //02	Identificação do processo ou ato concessório.
    fIND_PROC         : TACBrOrigemProcesso; //Indicador da origem do processo: 1 - Justiça Federal; 3 - Secretaria da Receita Federal do Brasil
  public
    property NUM_PROC : string              read FNUM_PROC write FNUM_PROC;
    property IND_PROC : TACBrOrigemProcesso read FIND_PROC write FIND_PROC;
  end;

  TRegistroF569List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF569;
    procedure SetItem(Index: Integer; const Value: TRegistroF569);
  public
    function New: TRegistroF569;
    property Items[Index: Integer]: TRegistroF569 read GetItem write SetItem;
  end;

  //REGISTRO F600: CONTRIBUIÇÃO RETIDA NA FONTE
  TRegistroF600 = class
  private
    FVL_RET                : currency;
    FVL_BC_RET             : currency;
    FVL_RET_COFINS         : currency;
    FVL_RET_PIS            : currency;
    FCOD_REC               : string;
    FIND_NAT_RET           : TACBrIndNatRetFonte;
    FCNPJ                  : string;
    FIND_NAT_REC           : TACBrIndNatRec;
    FIND_DEC               : string;
    FDT_RET                : TDateTime;
  public
    property IND_NAT_RET   : TACBrIndNatRetFonte read FIND_NAT_RET   write FIND_NAT_RET;
    property DT_RET        : TDateTime           read FDT_RET        write FDT_RET;
    property VL_BC_RET     : currency            read FVL_BC_RET     write FVL_BC_RET;
    property VL_RET        : currency            read FVL_RET        write FVL_RET;
    property COD_REC       : string              read FCOD_REC       write FCOD_REC;
    property IND_NAT_REC   : TACBrIndNatRec      read FIND_NAT_REC   write FIND_NAT_REC;
    property CNPJ          : string              read FCNPJ          write FCNPJ;
    property VL_RET_PIS    : currency            read FVL_RET_PIS    write FVL_RET_PIS;
    property VL_RET_COFINS : currency            read FVL_RET_COFINS write FVL_RET_COFINS;
    property IND_DEC       : string              read FIND_DEC       write FIND_DEC;
  end;

  // Registro F600 - Lista
  TRegistroF600List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF600;
    procedure SetItem(Index: Integer; const Value: TRegistroF600);
  public
    function New: TRegistroF600;
    property Items[Index: Integer]: TRegistroF600 read GetItem write SetItem;
  end;

  //REGISTRO F700: DEDUÇÕES DIVERSAS
  TRegistroF700 = class
  private
    FVL_DED_COFINS         : currency;
    FVL_DED_PIS            : currency;
    FIND_ORI_DED           : TACBrIndOrigemDiversas;
    FIND_NAT_DED           : TACBrIndNatDeducao;
    FVL_BC_OPER            : Currency;
    FCNPJ                  : string;
    FINF_COMP              : string;

  public
    property IND_ORI_DED   : TACBrIndOrigemDiversas read FIND_ORI_DED   write FIND_ORI_DED;
    property IND_NAT_DED   : TACBrIndNatDeducao     read FIND_NAT_DED   write FIND_NAT_DED;
    property VL_DED_PIS    : currency               read FVL_DED_PIS    write FVL_DED_PIS;
    property VL_DED_COFINS : currency               read FVL_DED_COFINS write FVL_DED_COFINS;
    property VL_BC_OPER    : currency               read FVL_BC_OPER    write FVL_BC_OPER;
    property CNPJ          : string                 read FCNPJ          write FCNPJ;
    property INF_COMP      : string                 read FINF_COMP      write FINF_COMP;
  end;

  // Registro F700 - Lista
  TRegistroF700List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF700;
    procedure SetItem(Index: Integer; const Value: TRegistroF700);
  public
    function New: TRegistroF700;
    property Items[Index: Integer]: TRegistroF700 read GetItem write SetItem;
  end;

  //REGISTRO F800: CRÉDITOS DECORRENTES DE EVENTOS DE INCORPORAÇÃO, FUSÃO E CISÃO
  TRegistroF800 = class
  private
    FVL_CRED_COFINS         : currency;
    FVL_CRED_PIS            : currency;
    FPER_CRED_CIS           : currency;
    FIND_NAT_EVEN           : string;
    FCOD_CRED               : TACBrCodCred;
    FCNPJ_SUCED             : string;
    FPA_CONT_CRED           : string;
    FDT_EVEN                : TDateTime;
  public
    property IND_NAT_EVEN   : string       read FIND_NAT_EVEN   write FIND_NAT_EVEN;
    property DT_EVEN        : TDateTime    read FDT_EVEN        write FDT_EVEN;
    property CNPJ_SUCED     : string       read FCNPJ_SUCED     write FCNPJ_SUCED;
    property PA_CONT_CRED   : string       read FPA_CONT_CRED   write FPA_CONT_CRED;
    property COD_CRED       : TACBrCodCred read FCOD_CRED       write FCOD_CRED;
    property VL_CRED_PIS    : currency     read FVL_CRED_PIS    write FVL_CRED_PIS;
    property VL_CRED_COFINS : currency     read FVL_CRED_COFINS write FVL_CRED_COFINS;
    property PER_CRED_CIS   : currency     read FPER_CRED_CIS   write FPER_CRED_CIS;
  end;

  // Registro F800 - Lista
  TRegistroF800List = class(TObjectList)
  private
    function  GetItem(Index: Integer): TRegistroF800;
    procedure SetItem(Index: Integer; const Value: TRegistroF800);
  public
    function New: TRegistroF800;
    property Items[Index: Integer]: TRegistroF800 read GetItem write SetItem;
  end;

  //REGISTRO F990: ENCERRAMENTO DO BLOCO F
  TRegistroF990 = class
  private
    FQTD_LIN_F: integer;
  public
    property QTD_LIN_F: integer read FQTD_LIN_F write FQTD_LIN_F;
  end;

implementation

{TRegistroF001}

constructor TRegistroF001.Create;
begin
  inherited Create;
  FRegistroF010 := TRegistroF010List.Create;
end;

destructor TRegistroF001.Destroy;
begin
  FRegistroF010.Free;
  inherited;
end;

{TRegistroF010}

function TRegistroF010List.GetItem(Index: Integer): TRegistroF010;
begin
  Result := TRegistroF010(Inherited Items[Index]);
end;

function TRegistroF010List.New: TRegistroF010;
begin
  Result := TRegistroF010.Create;
  Add(Result);
end;

procedure TRegistroF010List.SetItem(Index: Integer; const Value: TRegistroF010);
begin
  Put(Index, Value);
end;

{ TRegistroF010 }

constructor TRegistroF010.Create;
begin
  FRegistroF100 := TRegistroF100List.Create;
  FRegistroF120 := TRegistroF120List.Create;
  FRegistroF130 := TRegistroF130List.Create;
  FRegistroF150 := TRegistroF150List.Create;
  FRegistroF200 := TRegistroF200List.Create;
  FRegistroF500 := TRegistroF500List.Create;
  FRegistroF510 := TRegistroF510List.Create;
  FRegistroF525 := TRegistroF525List.Create;
  FRegistroF550 := TRegistroF550List.Create;
  FRegistroF560 := TRegistroF560List.Create;
  FRegistroF600 := TRegistroF600List.Create;
  FRegistroF700 := TRegistroF700List.Create;
  FRegistroF800 := TRegistroF800List.Create;
end;

destructor TRegistroF010.Destroy;
begin
  FRegistroF100.Free;
  FRegistroF120.Free;
  FRegistroF130.Free;
  FRegistroF150.Free;
  FRegistroF200.Free;
  FRegistroF550.Free;
  FRegistroF560.Free;
  FRegistroF500.Free;
  FRegistroF510.Free;
  FRegistroF525.Free;
  FRegistroF600.Free;
  FRegistroF700.Free;
  FRegistroF800.Free;
  inherited;
end;

{TRegistroF100}

function TRegistroF100List.GetItem(Index: Integer): TRegistroF100;
begin
  Result := TRegistroF100(Inherited Items[Index]);
end;

function TRegistroF100List.New: TRegistroF100;
begin
  Result := TRegistroF100.Create;
  Add(Result);
end;

procedure TRegistroF100List.SetItem(Index: Integer; const Value: TRegistroF100);
begin
  Put(Index, Value);
end;

{ TRegistroF100 }

constructor TRegistroF100.Create;
begin
  FRegistroF111 := TRegistroF111List.Create;
end;

destructor TRegistroF100.Destroy;
begin
  FRegistroF111.Free;
  inherited;
end;

{TRegistroF111}

function TRegistroF111List.GetItem(Index: Integer): TRegistroF111;
begin
  Result := TRegistroF111(Inherited Items[Index]);
end;

function TRegistroF111List.New: TRegistroF111;
begin
  Result := TRegistroF111.Create;
  Add(Result);
end;

procedure TRegistroF111List.SetItem(Index: Integer; const Value: TRegistroF111);
begin
  Put(Index, Value);
end;

{TRegistroF120}

function TRegistroF120List.GetItem(Index: Integer): TRegistroF120;
begin
  Result := TRegistroF120(Inherited Items[Index]);
end;

function TRegistroF120List.New: TRegistroF120;
begin
  Result := TRegistroF120.Create;
  Add(Result);
end;

procedure TRegistroF120List.SetItem(Index: Integer; const Value: TRegistroF120);
begin
  Put(Index, Value);
end;

{ TRegistroF120 }

constructor TRegistroF120.Create;
begin
  FRegistroF129 := TRegistroF129List.Create;
end;

destructor TRegistroF120.Destroy;
begin
  FRegistroF129.Free;
  inherited;
end;

{TRegistroF129}

function TRegistroF129List.GetItem(Index: Integer): TRegistroF129;
begin
  Result := TRegistroF129(Inherited Items[Index]);
end;

function TRegistroF129List.New: TRegistroF129;
begin
  Result := TRegistroF129.Create;
  Add(Result);
end;

procedure TRegistroF129List.SetItem(Index: Integer; const Value: TRegistroF129);
begin
  Put(Index, Value);
end;

{TRegistroF130}

function TRegistroF130List.GetItem(Index: Integer): TRegistroF130;
begin
  Result := TRegistroF130(Inherited Items[Index]);
end;

function TRegistroF130List.New: TRegistroF130;
begin
  Result := TRegistroF130.Create;
  Add(Result);
end;

procedure TRegistroF130List.SetItem(Index: Integer; const Value: TRegistroF130);
begin
  Put(Index, Value);
end;

{ TRegistroF130 }

constructor TRegistroF130.Create;
begin
  FRegistroF139 := TRegistroF139List.Create;
end;

destructor TRegistroF130.Destroy;
begin
  FRegistroF139.Free;
  inherited;
end;

{TRegistroF139}

function TRegistroF139List.GetItem(Index: Integer): TRegistroF139;
begin
  Result := TRegistroF139(Inherited Items[Index]);
end;

function TRegistroF139List.New: TRegistroF139;
begin
  Result := TRegistroF139.Create;
  Add(Result);
end;

procedure TRegistroF139List.SetItem(Index: Integer; const Value: TRegistroF139);
begin
  Put(Index, Value);
end;

{TRegistroF150}

function TRegistroF150List.GetItem(Index: Integer): TRegistroF150;
begin
  Result := TRegistroF150(Inherited Items[Index]);
end;

function TRegistroF150List.New: TRegistroF150;
begin
  Result := TRegistroF150.Create;
  Add(Result);
end;

procedure TRegistroF150List.SetItem(Index: Integer; const Value: TRegistroF150);
begin
  Put(Index, Value);
end;

{TRegistroF200}

function TRegistroF200List.GetItem(Index: Integer): TRegistroF200;
begin
  Result := TRegistroF200(Inherited Items[Index]);
end;

function TRegistroF200List.New: TRegistroF200;
begin
  Result := TRegistroF200.Create;
  Add(Result);
end;

procedure TRegistroF200List.SetItem(Index: Integer; const Value: TRegistroF200);
begin
  Put(Index, Value);
end;

{ TRegistroF200 }

constructor TRegistroF200.Create;
begin
  FRegistroF205 := TRegistroF205.Create;
  FRegistroF210 := TRegistroF210List.Create;
  FRegistroF211 := TRegistroF211List.Create;
end;

destructor TRegistroF200.Destroy;
begin
  FRegistroF205.Free;
  FRegistroF210.Free;
  FRegistroF211.Free;
  inherited;
end;

{TRegistroF210}

function TRegistroF210List.GetItem(Index: Integer): TRegistroF210;
begin
  Result := TRegistroF210(Inherited Items[Index]);
end;

function TRegistroF210List.New: TRegistroF210;
begin
  Result := TRegistroF210.Create;
  Add(Result);
end;

procedure TRegistroF210List.SetItem(Index: Integer; const Value: TRegistroF210);
begin
  Put(Index, Value);
end;

{TRegistroF211}

function TRegistroF211List.GetItem(Index: Integer): TRegistroF211;
begin
  Result := TRegistroF211(Inherited Items[Index]);
end;

function TRegistroF211List.New: TRegistroF211;
begin
  Result := TRegistroF211.Create;
  Add(Result);
end;

procedure TRegistroF211List.SetItem(Index: Integer; const Value: TRegistroF211);
begin
  Put(Index, Value);
end;

{TRegistroF600}

function TRegistroF600List.GetItem(Index: Integer): TRegistroF600;
begin
  Result := TRegistroF600(Inherited Items[Index]);
end;

function TRegistroF600List.New: TRegistroF600;
begin
  Result := TRegistroF600.Create;
  Add(Result);
end;

procedure TRegistroF600List.SetItem(Index: Integer; const Value: TRegistroF600);
begin
  Put(Index, Value);
end;

{TRegistroF700}

function TRegistroF700List.GetItem(Index: Integer): TRegistroF700;
begin
  Result := TRegistroF700(Inherited Items[Index]);
end;

function TRegistroF700List.New: TRegistroF700;
begin
  Result := TRegistroF700.Create;
  Add(Result);
end;

procedure TRegistroF700List.SetItem(Index: Integer; const Value: TRegistroF700);
begin
  Put(Index, Value);
end;

{TRegistroF800}

function TRegistroF800List.GetItem(Index: Integer): TRegistroF800;
begin
  Result := TRegistroF800(Inherited Items[Index]);
end;

function TRegistroF800List.New: TRegistroF800;
begin
  Result := TRegistroF800.Create;
  Add(Result);
end;

procedure TRegistroF800List.SetItem(Index: Integer; const Value: TRegistroF800);
begin
  Put(Index, Value);
end;

{ TRegistroF550List }

function TRegistroF550List.GetItem(Index: Integer): TRegistroF550;
begin
  Result := TRegistroF550(Inherited Items[Index]);
end;

function TRegistroF550List.New: TRegistroF550;
begin
  Result := TRegistroF550.Create;
  Add(Result);
end;

procedure TRegistroF550List.SetItem(Index: Integer;
  const Value: TRegistroF550);
begin
  Put(Index, Value);
end;

{ TRegistroF500List }

function TRegistroF500List.GetItem(Index: Integer): TRegistroF500;
begin
  Result := TRegistroF500(Inherited Items[Index]);
end;

function TRegistroF500List.New: TRegistroF500;
begin
  Result := TRegistroF500.Create;
  Add(Result);
end;

procedure TRegistroF500List.SetItem(Index: Integer;
  const Value: TRegistroF500);
begin
  Put(Index, Value);
end;

{ TRegistroF560List }

function TRegistroF560List.GetItem(Index: Integer): TRegistroF560;
begin
  Result := TRegistroF560(Inherited Items[Index]);
end;

function TRegistroF560List.New: TRegistroF560;
begin
  Result := TRegistroF560.Create;
  Add(Result);
end;

procedure TRegistroF560List.SetItem(Index: Integer;
  const Value: TRegistroF560);
begin
  Put(Index, Value);
end;

{ TRegistroF510List }

function TRegistroF510List.GetItem(Index: Integer): TRegistroF510;
begin
  Result := TRegistroF510(Inherited Items[Index]);
end;

function TRegistroF510List.New: TRegistroF510;
begin
  Result := TRegistroF510.Create;
  Add(Result);
end;

procedure TRegistroF510List.SetItem(Index: Integer;
  const Value: TRegistroF510);
begin
  Put(Index, Value);
end;


{ TRegistroF525List }

function TRegistroF525List.GetItem(Index: Integer): TRegistroF525;
begin
  Result := TRegistroF525(Inherited Items[Index]);
end;

function TRegistroF525List.New: TRegistroF525;
begin
  Result := TRegistroF525.Create;
  Add(Result);
end;

procedure TRegistroF525List.SetItem(Index: Integer;
  const Value: TRegistroF525);
begin
  Put(Index, Value);
end;

{ TRegistroF509List }

function TRegistroF509List.GetItem(Index: Integer): TRegistroF509;
begin
  Result := TRegistroF509(Inherited Items[Index]);
end;

function TRegistroF509List.New: TRegistroF509;
begin
  Result := TRegistroF509.Create;
  Add(Result);
end;

procedure TRegistroF509List.SetItem(Index: Integer; const Value: TRegistroF509);
begin
  Put(Index, Value);
end;

{ TRegistroF519List }

function TRegistroF519List.GetItem(Index: Integer): TRegistroF519;
begin
  Result := TRegistroF519(Inherited Items[Index]);
end;

function TRegistroF519List.New: TRegistroF519;
begin
  Result := TRegistroF519.Create;
  Add(Result);
end;

procedure TRegistroF519List.SetItem(Index: Integer; const Value: TRegistroF519);
begin
  Put(Index, Value);
end;

{ TRegistroF559List }

function TRegistroF559List.GetItem(Index: Integer): TRegistroF559;
begin
  Result := TRegistroF559(Inherited Items[Index]);
end;

function TRegistroF559List.New: TRegistroF559;
begin
  Result := TRegistroF559.Create;
  Add(Result);
end;

procedure TRegistroF559List.SetItem(Index: Integer; const Value: TRegistroF559);
begin
  Put(Index, Value);
end;

{ TRegistroF569List }

function TRegistroF569List.GetItem(Index: Integer): TRegistroF569;
begin
  Result := TRegistroF569(Inherited Items[Index]);
end;

function TRegistroF569List.New: TRegistroF569;
begin
  Result := TRegistroF569.Create;
  Add(Result);
end;

procedure TRegistroF569List.SetItem(Index: Integer; const Value: TRegistroF569);
begin
  Put(Index, Value);
end;

{ TRegistroF500 }

constructor TRegistroF500.Create;
begin
  FRegistroF509 := TRegistroF509List.Create;
end;

destructor TRegistroF500.Destroy;
begin
  FRegistroF509.Free;
  inherited;
end;

{ TRegistroF510 }

constructor TRegistroF510.Create;
begin
  FRegistroF519 := TRegistroF519List.Create;
end;

destructor TRegistroF510.Destroy;
begin
  FRegistroF519.Free;
  inherited;
end;

{ TRegistroF550 }

constructor TRegistroF550.Create;
begin
  FRegistroF559 := TRegistroF559List.Create;
end;

destructor TRegistroF550.Destroy;
begin
  FRegistroF559.Free;
  inherited;
end;

{ TRegistroF560 }

constructor TRegistroF560.Create;
begin
  FRegistroF569 := TRegistroF569List.Create;
end;

destructor TRegistroF560.Destroy;
begin
  FRegistroF569.Free;
  inherited;
end;

end.
