{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: João Pedro R Costa                              }
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
{******************************************************************************}

unit ACBrDeSTDABloco_0;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBRDeSTDABlocos;

type
  TRegistro0002List  = class;
  TRegistro0005      = class;
  TRegistro0030      = class;
  TRegistro0100      = class;

  /// Registro 0000 - ABERTURA DO ARQUIVO DIGITAL E IDENTIFICAÇÃO DA ENTIDADE

  TRegistro0000 = class
  private
    FDT_INI    : TDateTime;                /// Data inicial das informações contidas no arquivo
    FDT_FIN    : TDateTime;                /// Data Final das informações contidas no arquivo
    FNOME_EMPR : String;                   /// Nome empresarial do contribuinte:
    FCNPJ      : String;                   /// CNPJ do contribuinte (vedado informar CPF)
    FUF        : String;                   /// Sigla da unidade da Federação do domicílio fiscal do contribuinte
    FIE        : String;                   /// Inscrição Estadual do contribuinte:
    FCOD_MUN   : integer;                  /// Código do município do domicílio fiscal do contribuinte, conforme a tabela IBGE
    fIM        : String;                   /// Inscrição Municipal do contribuinte:
    FVAZIO1    : string;                   /// Texto fixo contendo ""
    FSUFRAMA   : String;                   /// Inscrição do contribuinte na Suframa
    FCOD_VER   : TACBrVersaoLeiaute;       /// Código da versão do leiaute: 100, 101, 102
    FCOD_FIN   : TACBrCodFinalidade;       /// Código da finalidade do arquivo, conforme a tabela indicada no item 3.2.1
    FCOD_CTD   : TACBrConteudoArquivo;     /// Código do conteúdo do arquivo, conforme a tabela indicada no item 3.2.2
    FPAIS      : string;                   /// Texto fixo contendo "Brasil"
    FFANTASIA  : string;                   /// Nome de fantasia associado ao nome empresarial
    FNIRE      : string;                   /// Número de Identificação do Registro de Empresas da Junta Comercial
    FCPF       : string;                   /// CPF do contribuinte (vedado informar CNPJ)
    FVAZIO2    : string;                   /// Texto fixo contendo ""
  public
    property DT_INI   : TDateTime            read FDT_INI    write fDT_INI;
    property DT_FIN   : TDateTime            read FDT_FIN    write fDT_FIN;
    property NOME_EMPR: String               read FNOME_EMPR write FNOME_EMPR;
    property CNPJ     : String               read FCNPJ      write FCNPJ;
    property UF       : String               read FUF        write FUF;
    property IE       : String               read FIE        write FIE;
    property COD_MUN  : integer              read FCOD_MUN   write FCOD_MUN;
    property IM       : String               read FIM        write FIM;
    property VAZIO1   : String               read FVAZIO1    write FVAZIO1;
    property SUFRAMA  : String               read FSUFRAMA   write FSUFRAMA;
    property COD_VER  : TACBrVersaoLeiaute   read FCOD_VER   write FCOD_VER;
    property COD_FIN  : TACBrCodFinalidade   read FCOD_FIN   write FCOD_FIN;
    property COD_CTD  : TACBrConteudoArquivo read FCOD_CTD   write FCOD_CTD;
    property PAIS     : String               read FPAIS      write FPAIS;
    property FANTASIA : String               read FFANTASIA  write FFANTASIA;
    property NIRE     : String               read FNIRE      write FNIRE;
    property CPF      : String               read FCPF       write FCPF;
    property VAZIO2   : String               read FVAZIO2    write FVAZIO2;
  end;

  /// REGISTRO 0001: ABERTURA DO BLOCO 0

  TRegistro0001 = class(TOpenBlocos)
  private
    FRegistro0002: TRegistro0002List;
    FRegistro0005: TRegistro0005;
    FRegistro0030: TRegistro0030;
    FRegistro0100: TRegistro0100;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property Registro0002: TRegistro0002List read FRegistro0002 write FRegistro0002;
    property Registro0005: TRegistro0005     read FRegistro0005 write FRegistro0005;
    property Registro0030: TRegistro0030     read FRegistro0030 write FRegistro0030;
    property Registro0100: TRegistro0100     read FRegistro0100 write FRegistro0100;
  end;

  /// REGISTRO 0002: INSCRIÇÕES ESTADUAIS COMO CONTRIBUINTE SUBSTITUTOS EM OUTRA UF

  TRegistro0002 = class
  private
    FUF   : String;    /// Sigla da unidade da Federação do domicílio fiscal do contribuinte substituto
    FIE_ST: String;    /// Inscrição estadual do participante emitente contribuinte-substituto na unidade da Federação do destinatário substituído
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property UF   : string read FUF    write FUF;
    property IE_ST: string read FIE_ST write FIE_ST;
  end;

  /// REGISTRO 0002 - Lista

  TRegistro0002List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistro0002;
    procedure SetItem(Index: Integer; const Value: TRegistro0002);
  public
    function New(AOwner: TRegistro0001): TRegistro0002;
    property Items[Index: Integer]: TRegistro0002 read GetItem write SetItem;
  end;

  /// REGISTRO 0005: DADOS COMPLEMENTARES DO CONTRIBUINTE
  TRegistro0005 = class
  private
    FNOME_RESP: string;              // Nome do responsável
    FCOD_ASSIN: TACBrAssinante;      // Código de qualificação do assinante, conforme a tabela indicada no item 3.3.2
    FCPF_RESP : string;              // CPF do responsável
    FCEP      : string;              // Código de Endereçamento Postal
    FENDR     : string;              // Endereço do imóvel
    FNUM      : string;              // Número do imóvel
    FCOMPL    : string;              // Dados complementares do endereço
    FBAIRRO   : string;              // Bairro em que o imóvel está situado
    FCEP_CP   : string;              // Código de Endereçamento Postal da caixa postal
    FCP       : Integer;             // Caixa postal
    FFONE     : string;              // Número do telefone
    FFAX      : string;              // Número do fax
    FEMAIL    : string;              // Endereço do correio eletrônico
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property NOME_RESP: string         read FNOME_RESP write FNOME_RESP;
    property COD_ASSIN: TACBrAssinante read FCOD_ASSIN write FCOD_ASSIN;
    property CPF_RESP : string         read FCPF_RESP  write FCPF_RESP;
    property CEP      : string         read FCEP       write FCEP;
    property ENDR     : string         read FENDR      write FENDR;
    property NUM      : string         read FNUM       write FNUM;
    property COMPL    : string         read FCOMPL     write FCOMPL;
    property BAIRRO   : string         read FBAIRRO    write FBAIRRO;
    property CEP_CP   : string         read FCEP_CP    write FCEP_CP;
    property CP       : Integer        read FCP        write FCP;
    property FONE     : string         read FFONE      write FFONE;
    property FAX      : string         read FFAX       write FFAX;
    property EMAIL    : string         read FEMAIL     write FEMAIL;
  end;

  /// REGISTRO 0030: PERFIL DO CONTRIBUINTE

  TRegistro0030 = class
  private
    FIND_ED  : TACBrIndEntrada;        /// Indicador de entrada de dados:
    FIND_ARQ : TACBrIndDocumentos;     /// Indicador do documento contido no arquivo
    FPRF_ISS : TACBrIndExigISS;        /// Indicador de exigibilidade da escrituração do ISS
    FPRF_ICMS: TACBrIndExigICMS;       /// Indicador de exigibilidade da escrituração do ICMS
    FPRF_RIDF: TACBrIndExigImpDocF;    /// Indicador de exigibilidade do Registro de Impressão de Documentos Fiscais
    FPRF_RUDF: TACBrIndExigUtilDocF;   /// Indicador de exigibilidade do Registro de Utilização de Documentos Fiscais
    FPRF_LMC : TACBrIndExigMovComb;    /// Indicador de exigibilidade do Livro de Movimentação de Combustíveis
    FPRF_RV  : TACBrIndExigRegVeic;    /// Indicador de exigibilidade do Registro de Veículos
    FPRF_RI  : TACBrIndExigRegInv;     /// Indicador de exigibilidade anual do Registro de Inventário
    FIND_EC  : TACBrIndApresEC;        /// Indicador de apresentação da escrituração contábil
    FIND_ISS : TACBrIndOpISS;          /// Indicador de operações sujeitas ao ISS
    FIND_RT  : TACBrIndOpISSRet;       /// Indicador de operações sujeitas à retenção tributária do ISS, na condição de contribuinte-substituído
    FIND_ICMS: TACBrIndOpICMS;         /// Indicador de operações sujeitas ao ICMS
    FIND_ST  : TACBrIndOpICMSST;       /// Indicador de operações sujeitas à substituição tributária do ICMS, na condição de contribuinte-substituto
    FIND_AT  : TACBrIndOpICMSAnt;      /// Indicador de operações sujeitas à antecipação tributária do ICMS, nas entradas
    FIND_IPI : TACBrIndOpIPI;          /// Indicador de operações sujeitas ao IPI
    FIND_RI  : TACBrIndApresAvlRegInv; /// Indicador de apresentação avulsa do Registro de Inventário
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property IND_ED  : TACBrIndEntrada        read FIND_ED   write FIND_ED;
    property IND_ARQ : TACBrIndDocumentos     read FIND_ARQ  write FIND_ARQ;
    property PRF_ISS : TACBrIndExigISS        read FPRF_ISS  write FPRF_ISS;
    property PRF_ICMS: TACBrIndExigICMS       read FPRF_ICMS write FPRF_ICMS;
    property PRF_RIDF: TACBrIndExigImpDocF    read FPRF_RIDF write FPRF_RIDF;
    property PRF_RUDF: TACBrIndExigUtilDocF   read FPRF_RUDF write FPRF_RUDF;
    property PRF_LMC : TACBrIndExigMovComb    read FPRF_LMC  write FPRF_LMC;
    property PRF_RV  : TACBrIndExigRegVeic    read FPRF_RV   write FPRF_RV;
    property PRF_RI  : TACBrIndExigRegInv     read FPRF_RI   write FPRF_RI;
    property IND_EC  : TACBrIndApresEC        read FIND_EC   write FIND_EC;
    property IND_ISS : TACBrIndOpISS          read FIND_ISS  write FIND_ISS;
    property IND_RT  : TACBrIndOpISSRet       read FIND_RT   write FIND_RT;
    property IND_ICMS: TACBrIndOpICMS         read FIND_ICMS write FIND_ICMS;
    property IND_ST  : TACBrIndOpICMSST       read FIND_ST   write FIND_ST;
    property IND_AT  : TACBrIndOpICMSAnt      read FIND_AT   write FIND_AT;
    property IND_IPI : TACBrIndOpIPI          read FIND_IPI  write FIND_IPI;
    property IND_RI  : TACBrIndApresAvlRegInv read FIND_RI   write FIND_RI;
  end;

  /// Registro 0100 - DADOS DO CONTABILISTA

  TRegistro0100 = class
  private
    FNOME     : String;              /// Nome do contabilista/escritório de contabilidade
    FCOD_ASSIN: TACBrAssinante;      /// Código de qualificação do assinante, conforme a tabela indicada no item 3.3.2
    FCNPJ     : String;              /// CNPJ do escritório de contabilidade
    FCPF      : String;              /// CPF do contabilista
    FCRC      : String;              /// CRC do contabilista
    FCEP      : String;              /// Código de Endereçamento Postal:
    FENDR     : String;              /// Endereço do imóvel
    FNUM      : String;              /// Número do imóvel:
    FCOMPL    : String;              /// Dados complementares do endereço:
    FBAIRRO   : String;              /// Bairro em que o imóvel está situado:
    FUF       : String;              /// Sigla da unidade da Federação do endereço do contabilista/escritório de contabilidade
    FCOD_MUN  : integer;             /// Código do município, conforme tabela externa IBGE:
    FCEP_CP   : string;              /// Código de Endereçamento Postal da caixa postal
    FCP       : Integer;             /// Caixa postal
    FFONE     : string;              /// Número do telefone
    FFAX      : string;              /// Número do fax
    FEMAIL    : string;              /// Endereço do correio eletrônico
  public
    constructor Create(AOwner: TRegistro0001); virtual; /// Create

    property NOME     : String         read FNOME      write FNOME;
    property COD_ASSIN: TACBrAssinante read FCOD_ASSIN write FCOD_ASSIN;
    property CNPJ     : String         read FCNPJ      write FCNPJ;
    property CPF      : String         read FCPF       write FCPF;
    property CRC      : String         read FCRC       write FCRC;
    property CEP      : String         read FCEP       write FCEP;
    property ENDR     : String         read FENDR      write FENDR;
    property NUM      : String         read FNUM       write FNUM;
    property COMPL    : String         read FCOMPL     write FCOMPL;
    property BAIRRO   : String         read FBAIRRO    write FBAIRRO;
    property UF       : String         read FUF        write FUF;
    property COD_MUN  : integer        read FCOD_MUN   write FCOD_MUN;
    property CEP_CP   : string         read FCEP_CP    write FCEP_CP;
    property CP       : Integer        read FCP        write FCP;
    property FONE     : string         read FFONE      write FFONE;
    property FAX      : string         read FFAX       write FFAX;
    property EMAIL    : string         read FEMAIL     write FEMAIL;
  end;

  /// Registro 0990 - ENCERRAMENTO DO BLOCO 0

  TRegistro0990 = class
  private
    fQTD_LIN_0: Integer; /// Quantidade total de linhas do Bloco 0
  public
    property QTD_LIN_0: Integer read fQTD_LIN_0 write fQTD_LIN_0;
  end;

implementation

{ TRegistro0001 }

constructor TRegistro0001.Create;
begin
  inherited Create;
  FRegistro0002 := TRegistro0002List.Create;
  FRegistro0005 := TRegistro0005.Create(Self);
  FRegistro0030 := TRegistro0030.Create(Self);
  FRegistro0100 := TRegistro0100.Create(Self);
  //
  IND_MOV := imSemDados;
end;

destructor TRegistro0001.Destroy;
begin
  FRegistro0002.Free;
  FRegistro0005.Free;
  FRegistro0030.Free;
  FRegistro0100.Free;

  inherited;
end;

{ TRegistro0002 }

constructor TRegistro0002.Create(AOwner: TRegistro0001);
begin

end;
    
{ TRegistro0002List }

function TRegistro0002List.GetItem(Index: Integer): TRegistro0002;
begin
  Result := TRegistro0002(Inherited Items[Index]);
end;

function TRegistro0002List.New(AOwner: TRegistro0001): TRegistro0002;
begin
  Result := TRegistro0002.Create(AOwner);
  Add(Result);
end;

procedure TRegistro0002List.SetItem(Index: Integer; const Value: TRegistro0002);
begin
  Put(Index, Value);
end;

{ TRegistro0005 }

constructor TRegistro0005.Create(AOwner: TRegistro0001);
begin

end;

{ TRegistro0030 }

constructor TRegistro0030.Create(AOwner: TRegistro0001);
begin

end;

{ TRegistro0100 }

constructor TRegistro0100.Create(AOwner: TRegistro0001);
begin

end;

end.

