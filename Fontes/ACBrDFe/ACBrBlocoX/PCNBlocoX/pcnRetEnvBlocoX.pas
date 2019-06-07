{******************************************************************************}
{ Projeto: Componente ACBrBlocoX                                               }
{ Biblioteca multiplataforma de componentes Delphi para Geração de arquivos    }
{ do Bloco X                                                                   }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{******************************************************************************}

{$I ACBr.inc}

unit pcnRetEnvBlocoX;

interface

uses
  SysUtils, Classes, contnrs, pcnConversao, pcnLeitor;

type

  { TRetEnvBlocoX }

  TRetEnvBlocoX = class(TPersistent)
  private
    fLeitor         : TLeitor;
    fVersao         : AnsiString;
    fSituacaoProcCod: Integer;
    fSituacaoProcStr: AnsiString;
    fRecibo         : AnsiString;
    fTipo           : AnsiString;
    fMensagem       : AnsiString;
    fDataRef        : AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor         : TLeitor    read fLeitor;
    property Versao         : AnsiString read fVersao;
    property SituacaoProcCod: Integer    read fSituacaoProcCod;
    property SituacaoProcStr: AnsiString read fSituacaoProcStr;
    property Recibo         : AnsiString read fRecibo;
    property Tipo           : AnsiString read fTipo;
    property Mensagem       : AnsiString read fMensagem;
    property DataRef        : AnsiString read fDataRef;
  end;

  { TRetTransmitirBlocoX }

  TRetTransmitirBlocoX = class(TPersistent)
  private
    FLeitor: TLeitor;
    FRecibo: AnsiString;
    FSituacaoProcCod: Integer;
    FSituacaoProcStr: AnsiString;
    FMensagem: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor:   TLeitor    read FLeitor;
    property Recibo:   AnsiString read FRecibo;
    property Mensagem: AnsiString read FMensagem;
    property SituacaoProcCod: Integer    read FSituacaoProcCod;
    property SituacaoProcStr: AnsiString read FSituacaoProcStr;
  end;

  { TRetEventoBlocoX }

  TRetEventoBlocoX = class(TPersistent)
  private
    FDataHora: TDateTime;
    FCodigo: Integer;
    FDescricao: AnsiString;
    FMotivo: AnsiString;
  public
    property Codigo:    Integer    read FCodigo;
    property DataHora:  TDateTime  read FDataHora;
    property Descricao: AnsiString read FDescricao;
    property Motivo:    AnsiString read FMotivo;
  end;

  { TRetEventoBlocoXCollection }

  TRetEventoBlocoXCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetEventoBlocoX;
    procedure SetItem(Index: Integer; Value: TRetEventoBlocoX);
  public
    function Add: TRetEventoBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetEventoBlocoX;
    property Items[Index: Integer]: TRetEventoBlocoX read GetItem write SetItem; default;
  end;

  { TRetConsultaArquivoBlocoX }

  TRetConsultaArquivoBlocoX = class(TPersistent)
    private
      FLeitor: TLeitor;
      FRecibo: AnsiString;
      FSituacaoProcCod: Integer;
      FSituacaoProcStr: AnsiString;
      FMensagem: AnsiString;
    public
      constructor Create;
      destructor Destroy; override;
      function LerXML: Boolean;
    published
      property Leitor: TLeitor read FLeitor;
      property Recibo: AnsiString read FRecibo;
      property SituacaoProcCod: Integer read FSituacaoProcCod;
      property SituacaoProcStr: AnsiString read FSituacaoProcStr;
      property Mensagem: AnsiString read FMensagem;
  end;

  { TRetReprocessarBlocoX }

  TRetReprocessarBlocoX = class(TPersistent)
  private
    FLeitor: TLeitor;
    FRecibo: AnsiString;
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FSituacaoProcCod: Integer;
    FSituacaoProcStr: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Recibo:          AnsiString read FRecibo;
    property Leitor:          TLeitor    read FLeitor;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property SituacaoProcCod: Integer    read FSituacaoProcCod;
    property SituacaoProcStr: AnsiString read FSituacaoProcStr;
  end;

  { TRetConsHistBlocoX }

  TRetConsHistBlocoX = class(TPersistent)
  private
    FLeitor: TLeitor;
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FEventos: TRetEventoBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;

    property Leitor:          TLeitor    read FLeitor;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;

    property Eventos: TRetEventoBlocoXCollection read FEventos;
  end;

  { TRetArquivoBlocoX }

  TRetArquivoBlocoX = class(TPersistent)
  private
    FRecibo: AnsiString;
    FHashArquivo: AnsiString;
    FNumeroFabrECF: AnsiString;
    FDataRef: TDateTime;
    FDataHoraRecepcao: TDateTime;
    FDataHoraProcess: TDateTime;
    FTipoRecepcaoCodigo: Integer;
    FTipoRecepcaoDescricao: AnsiString;
    FSituacaoProcCod: Integer;
    FSituacaoProcStr: AnsiString;
  public
    property Recibo:                AnsiString read FRecibo;
    property HashArquivo:           AnsiString read FHashArquivo;
    property NumeroFabrECF:         AnsiString read FNumeroFabrECF;
    property DataRef:               TDateTime  read FDataRef;
    property DataHoraRecepcao:      TDateTime  read FDataHoraRecepcao;
    property DataHoraProcess:       TDateTime  read FDataHoraProcess;
    property TipoRecepcaoDescricao: AnsiString read FTipoRecepcaoDescricao;
    property TipoRecepcaoCodigo:    Integer    read FTipoRecepcaoCodigo;
    property SituacaoProcCod:       Integer    read FSituacaoProcCod;
    property SituacaoProcStr:       AnsiString read FSituacaoProcStr;
  end;

  { TRetArquivoBlocoXCollection }

  TRetArquivoBlocoXCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetArquivoBlocoX;
    procedure SetItem(Index: Integer; Value: TRetArquivoBlocoX);
  public
    function Add: TRetArquivoBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetArquivoBlocoX;
    property Items[Index: Integer]: TRetArquivoBlocoX read GetItem write SetItem; default;
  end;

  { TRetListarBlocoX }

  TRetListarBlocoX = class(TPersistent)
  private
    FLeitor: TLeitor;
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FIE: AnsiString;
    FArquivos: TRetArquivoBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor:   TLeitor           read FLeitor;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property IE:       AnsiString        read FIE;

    property Arquivos: TRetArquivoBlocoXCollection read FArquivos;
  end;

  { TRetDownloadBlocoX }

  TRetDownloadBlocoX = class(TPersistent)
  private
    FLeitor: TLeitor;
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FArquivo: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor:          TLeitor    read FLeitor;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property Arquivo:         AnsiString read FArquivo;
  end;

  { TRetCancelarBlocoX }

  TRetCancelarBlocoX = class(TPersistent)
  private
    FLeitor: TLeitor;
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FRecibo: AnsiString;
    FSituacaoProcCod: Integer;
    FSituacaoProcStr: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor:          TLeitor    read FLeitor;
    property SituacaoOperCod: Integer    read FSituacaoOperCod;
    property SituacaoOperStr: AnsiString read FSituacaoOperStr;
    property Recibo:          AnsiString read FRecibo;
    property SituacaoProcCod: Integer    read FSituacaoProcCod;
    property SituacaoProcStr: AnsiString read FSituacaoProcStr;
  end;

  { TRetAvisoBlocoX }

  TRetAvisoBlocoX = class(TPersistent)
  private
    FCodigo: Integer;
    FDescricao: AnsiString;
  public
    property Codigo:    Integer    read FCodigo;
    property Descricao: AnsiString read FDescricao;
  end;

  { TRetAvisoBlocoXCollection }

  TRetAvisoBlocoXCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetAvisoBlocoX;
    procedure SetItem(Index: Integer; Value: TRetAvisoBlocoX);
  public
    function Add: TRetAvisoBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetAvisoBlocoX;
    property Items[Index: Integer]: TRetAvisoBlocoX read GetItem write SetItem; default;
  end;

  { TRetReciboBlocoX }

  TRetReciboBlocoX = class(TPersistent)
  private
    FRecibo: AnsiString;
  public
    property Recibo: AnsiString read FRecibo;
  end;

  { TRetReciboBlocoXCollection }

  TRetReciboBlocoXCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetReciboBlocoX;
    procedure SetItem(Index: Integer; Value: TRetReciboBlocoX);
  public
    function Add: TRetReciboBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetReciboBlocoX;
    property Items[Index: Integer]: TRetReciboBlocoX read GetItem write SetItem; default;
  end;

  { TRetPendenciaBlocoX }

  TRetPendenciaBlocoX = class(TPersistent)
  private
    FCodigo: Integer;
    FDescricao: AnsiString;
    FQuantidade: Integer;
    FRecibos: TRetReciboBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Codigo:     Integer    read FCodigo;
    property Descricao:  AnsiString read FDescricao;
    property Quantidade: Integer    read FQuantidade;
    property Recibos:    TRetReciboBlocoXCollection read FRecibos;
  end;

  { TRetPendenciaBlocoXCollection }

  TRetPendenciaBlocoXCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetPendenciaBlocoX;
    procedure SetItem(Index: Integer; Value: TRetPendenciaBlocoX);
  public
    function Add: TRetPendenciaBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetPendenciaBlocoX;
    property Items[Index: Integer]: TRetPendenciaBlocoX read GetItem write SetItem; default;
  end;

  { TRetECFBlocoX }

  TRetECFBlocoX = class(TPersistent)
  private
    FQtdAvisos: Integer;
    FQtdPendencias: Integer;
    FNumeroFabricacao: String;
    FSituacaoPafECFStr: String;
    FSituacaoPafECFCod: Integer;
    FAvisos: TRetAvisoBlocoXCollection;
    FPendencias: TRetPendenciaBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property NumeroFabricacao:  String  read FNumeroFabricacao;
    property SituacaoPafECFCod: Integer read FSituacaoPafECFCod;
    property SituacaoPafECFStr: String  read FSituacaoPafECFStr;
    property QtdPendencias:     Integer read FQtdPendencias;
    property QtdAvisos:         Integer read FQtdAvisos;
    property Pendencias: TRetPendenciaBlocoXCollection read FPendencias;
    property Avisos:     TRetAvisoBlocoXCollection     read FAvisos;
  end;

  { TRetECFBlocoXCollection }

  TRetECFBlocoXCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetECFBlocoX;
    procedure SetItem(Index: Integer; Value: TRetECFBlocoX);
  public
    function Add: TRetECFBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetECFBlocoX;
    property Items[Index: Integer]: TRetECFBlocoX read GetItem write SetItem; default;
  end;

  { TRetConsPendContrReducoesZBlocoX }

  TRetConsPendContrReducoesZBlocoX = class(TPersistent)
  private
    FECFs: TRetECFBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property ECFs: TRetECFBlocoXCollection read FECFs;
  end;

  { TRetConsPendContrEstoqueBlocoX }

  TRetConsPendContrEstoqueBlocoX = class(TPersistent)
  private
    FDataReferencia: TDateTime;
    FPendencias: TRetPendenciaBlocoXCollection;
    FAvisos: TRetAvisoBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property DataReferencia: TDateTime read FDataReferencia;
    property Pendencias: TRetPendenciaBlocoXCollection read FPendencias;
    property Avisos:     TRetAvisoBlocoXCollection     read FAvisos;
  end;

  { TRetConsPendContrEstoqueBlocoXCollection }

  TRetConsPendContrEstoqueBlocoXCollection = class(TObjectList)
  private
    FQtdPendencias: Integer;
    FQtdAvisos: Integer;

    function GetItem(Index: Integer): TRetConsPendContrEstoqueBlocoX;
    procedure SetItem(Index: Integer; Value: TRetConsPendContrEstoqueBlocoX);
  public
    function Add: TRetConsPendContrEstoqueBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetConsPendContrEstoqueBlocoX;

    property QtdPendencias: Integer read FQtdPendencias;
    property QtdAvisos:     Integer read FQtdAvisos;
    property Items[Index: Integer]: TRetConsPendContrEstoqueBlocoX read GetItem write SetItem; default;
  end;

  { TRetConsPendContribuinteBlocoX }

  TRetConsPendContribuinteBlocoX = class(TPersistent)
  private
    FIE: string;
    FLeitor: TLeitor;
    FDataObrigacao: TDateTime;
    FSituacaoOperCod: Integer;
    FTransmiteEstoque: Boolean;
    FSituacaoOperStr: string;
    FReducaoZ: TRetConsPendContrReducoesZBlocoX;
    FEstoques: TRetConsPendContrEstoqueBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;

    property Leitor:           TLeitor   read FLeitor;
    property SituacaoOperCod:  Integer   read FSituacaoOperCod;
    property SituacaoOperStr:  String    read FSituacaoOperStr;
    property IE:               String    read FIE;
    property DataObrigacao:    TDateTime read FDataObrigacao;
    property TransmiteEstoque: Boolean   read FTransmiteEstoque;

    property ReducaoZ: TRetConsPendContrReducoesZBlocoX         read FReducaoZ;
    property Estoques: TRetConsPendContrEstoqueBlocoXCollection read FEstoques;
  end;

  { TRetEstabelecimentoBlocoX }

  TRetEstabelecimentoBlocoX = class(TPersistent)
  private
    FIE: AnsiString;
    FQtdAvisos: Integer;
    FQtdPendencias: Integer;
    FDataObrigacao: TDateTime;
    FTransmiteEstoque: Boolean;
    FNomeEmpresarial: AnsiString;
  public
    property IE:               AnsiString read FIE;
    property NomeEmpresarial:  AnsiString read FNomeEmpresarial;
    property DataObrigacao:    TDateTime  read FDataObrigacao;
    property TransmiteEstoque: Boolean    read FTransmiteEstoque;
    property QtdPendencias:    Integer    read FQtdPendencias;
    property QtdAvisos:        Integer    read FQtdAvisos;
  end;

  { TRetEstabelecimentoBlocoXCollection }

  TRetEstabelecimentoBlocoXCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TRetEstabelecimentoBlocoX;
    procedure SetItem(Index: Integer; Value: TRetEstabelecimentoBlocoX);
  public
    function Add: TRetEstabelecimentoBlocoX; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetEstabelecimentoBlocoX;
    property Items[Index: Integer]: TRetEstabelecimentoBlocoX read GetItem write SetItem; default;
  end;

  { TRetConsPendDesenvolvedorPafEcfBlocoX }

  TRetConsPendDesenvolvedorPafEcfBlocoX = class(TPersistent)
  private
    FLeitor: TLeitor;
    FSituacaoOperCod: Integer;
    FSituacaoOperStr: AnsiString;
    FEstabelecimentos : TRetEstabelecimentoBlocoXCollection;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor:           TLeitor    read FLeitor;
    property SituacaoOperCod:  Integer    read FSituacaoOperCod;
    property SituacaoOperStr:  AnsiString read FSituacaoOperStr;
    property Estabelecimentos: TRetEstabelecimentoBlocoXCollection read FEstabelecimentos;
  end;

implementation

{ TRetEnvBlocoX }

constructor TretEnvBlocoX.Create;
begin
  inherited;
  fLeitor := TLeitor.Create;
end;

destructor TretEnvBlocoX.Destroy;
begin
  fLeitor.Free;
  inherited;
end;

function TretEnvBlocoX.LerXml: Boolean;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'Resposta') <> '') then
    begin
      fVersao          := Leitor.rAtributo('Versao');
      fSituacaoProcCod := Leitor.rCampo(tcInt, 'SituacaoProcessamentoCodigo');
      fSituacaoProcStr := Leitor.rCampo(tcStr, 'SituacaoProcessamentoDescricao');
      fRecibo          := Leitor.rCampo(tcStr, 'Recibo');
      fTipo            := Leitor.rCampo(tcStr, 'Tipo');
      fMensagem        := Leitor.rCampo(tcStr, 'Mensagem');
      fDataRef         := Leitor.rCampo(tcStr, 'DataReferencia');
      
      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetEventoBlocoXCollection }

function TRetEventoBlocoXCollection.GetItem(Index: Integer): TRetEventoBlocoX;
begin 
  Result := TRetEventoBlocoX(inherited GetItem(Index));
end;

procedure TRetEventoBlocoXCollection.SetItem(Index: Integer;
  Value: TRetEventoBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetEventoBlocoXCollection.Add: TRetEventoBlocoX;
begin
  Result := Self.New;
end;

function TRetEventoBlocoXCollection.New: TRetEventoBlocoX;
begin
  Result := TRetEventoBlocoX.Create;
  Self.Add(Result);
end;

{ TRetConsHistBlocoX }

constructor TRetConsHistBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
  FEventos := TRetEventoBlocoXCollection.Create;
end;

destructor TRetConsHistBlocoX.Destroy;
begin
  FLeitor.Free;
  FEventos.Free;
  inherited;
end;

function TRetConsHistBlocoX.LerXml: Boolean;
var
  I: Integer;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaConsultarHistoricoArquivo') <> '') then
    begin
      FSituacaoOperCod := Leitor.rCampo(tcInt, 'SituacaoOperacaoCodigo');
      FSituacaoOperStr := Leitor.rCampo(tcStr, 'SituacaoOperacaoDescricao');

      if (Leitor.rExtrai(2, 'Eventos') <> '') then
      begin
        I := 0;
        while (Leitor.rExtrai(3, 'Evento', '', I + 1) <> '') do
        begin
          with FEventos.New do
          begin                                               
            FCodigo    := Leitor.rCampo(tcInt, 'Codigo');      
            FMotivo    := Leitor.rCampo(tcStr, 'Motivo');     
            FDescricao := Leitor.rCampo(tcStr, 'Descricao');
            FDataHora  := Leitor.rCampo(tcDatHor, 'DataHora');
          end;

          Inc(I);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetArquivoBlocoXCollection }

function TRetArquivoBlocoXCollection.GetItem(Index: Integer): TRetArquivoBlocoX;
begin
  Result := TRetArquivoBlocoX(inherited GetItem(Index));
end;

procedure TRetArquivoBlocoXCollection.SetItem(Index: Integer;
  Value: TRetArquivoBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetArquivoBlocoXCollection.Add: TRetArquivoBlocoX;
begin
  Result := Self.New;
end;

function TRetArquivoBlocoXCollection.New: TRetArquivoBlocoX;
begin
  Result := TRetArquivoBlocoX.Create;
  Self.Add(Result);
end;

{ TRetListarBlocoX }

constructor TRetListarBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
  FArquivos := TRetArquivoBlocoXCollection.Create;
end;

destructor TRetListarBlocoX.Destroy;
begin
  FLeitor.Free;
  FArquivos.Free;
  inherited;
end;

function TRetListarBlocoX.LerXml: Boolean;
var
  I: Integer;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaListarArquivos') <> '') then
    begin
      FSituacaoOperCod := Leitor.rCampo(tcInt, 'SituacaoOperacaoCodigo');
      FSituacaoOperStr := Leitor.rCampo(tcStr, 'SituacaoOperacaoDescricao');
      FIE              := Leitor.rCampo(tcStr, 'IE');

      if (Leitor.rExtrai(2, 'Arquivos') <> '') then
      begin
        I := 0;
        while (Leitor.rExtrai(3, 'Arquivo', '', I + 1) <> '') do
        begin
          with FArquivos.New do
          begin
            FRecibo                := Leitor.rCampo(tcStr, 'Recibo');
            FHashArquivo           := Leitor.rCampo(tcStr, 'HashArquivo');
            FNumeroFabrECF         := Leitor.rCampo(tcStr, 'NumeroFabricacaoEcf');
            FDataRef               := Leitor.rCampo(tcDat, 'DataReferencia');
            FDataHoraRecepcao      := Leitor.rCampo(tcDatHor, 'DataRecepcao');
            FDataHoraProcess       := Leitor.rCampo(tcDatHor, 'DataProcessamento');
            FTipoRecepcaoCodigo    := Leitor.rCampo(tcInt, 'TipoRecepcaoCodigo');
            FTipoRecepcaoDescricao := Leitor.rCampo(tcStr, 'TipoRecepcaoDescricao');
            FSituacaoProcCod       := Leitor.rCampo(tcInt, 'SituacaoProcessamentoCodigo');
            FSituacaoProcStr       := Leitor.rCampo(tcStr, 'SituacaoProcessamentoDescricao');
          end;
          Inc(I);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetDownloadBlocoX }

constructor TRetDownloadBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
end;

destructor TRetDownloadBlocoX.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetDownloadBlocoX.LerXml: Boolean;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaDownloadArquivo') <> '') then
    begin
      FSituacaoOperCod := Leitor.rCampo(tcInt, 'SituacaoOperacaoCodigo');
      FSituacaoOperStr := Leitor.rCampo(tcStr, 'SituacaoOperacaoDescricao');
      FArquivo         := Leitor.rCampo(tcStr, 'Arquivo');
      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetCancelarBlocoX }

constructor TRetCancelarBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
end;

destructor TRetCancelarBlocoX.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetCancelarBlocoX.LerXml: Boolean;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaManutencao') <> '') then
    begin
      FSituacaoOperCod := Leitor.rCampo(tcInt, 'SituacaoOperacaoCodigo');
      FSituacaoOperStr := Leitor.rCampo(tcStr, 'SituacaoOperacaoDescricao');
      FRecibo          := Leitor.rCampo(tcStr, 'Recibo');
      FSituacaoProcCod := Leitor.rCampo(tcInt, 'SituacaoProcessamentoCodigo');
      FSituacaoProcStr := Leitor.rCampo(tcStr, 'SituacaoProcessamentoDescricao');
      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetReprocessarBlocoX }

constructor TRetReprocessarBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
end;

destructor TRetReprocessarBlocoX.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetReprocessarBlocoX.LerXml: Boolean;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaManutencao') <> '') then
    begin
      FSituacaoOperCod := Leitor.rCampo(tcInt, 'SituacaoOperacaoCodigo');
      FSituacaoOperStr := Leitor.rCampo(tcStr, 'SituacaoOperacaoDescricao');
      FRecibo          := Leitor.rCampo(tcStr, 'Recibo');
      FSituacaoProcCod := Leitor.rCampo(tcInt, 'SituacaoProcessamentoCodigo');
      FSituacaoProcStr := Leitor.rCampo(tcStr, 'SituacaoProcessamentoDescricao');
      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetAvisoBlocoXCollection }

function TRetAvisoBlocoXCollection.GetItem(Index: Integer): TRetAvisoBlocoX;
begin
  Result := TRetAvisoBlocoX(inherited GetItem(Index));
end;

procedure TRetAvisoBlocoXCollection.SetItem(Index: Integer;
  Value: TRetAvisoBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetAvisoBlocoXCollection.Add: TRetAvisoBlocoX;
begin
  Result := Self.New;
end;

function TRetAvisoBlocoXCollection.New: TRetAvisoBlocoX;
begin
  Result := TRetAvisoBlocoX.Create;
  Self.Add(Result);
end;

{ TRetReciboBlocoXCollection }

function TRetReciboBlocoXCollection.GetItem(Index: Integer): TRetReciboBlocoX;
begin
  Result := TRetReciboBlocoX(inherited GetItem(Index));
end;

procedure TRetReciboBlocoXCollection.SetItem(Index: Integer;
  Value: TRetReciboBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetReciboBlocoXCollection.Add: TRetReciboBlocoX;
begin
  Result := Self.New;
end;

function TRetReciboBlocoXCollection.New: TRetReciboBlocoX;
begin
  Result := TRetReciboBlocoX.Create;
  Self.Add(Result);
end;

{ TRetPendenciaBlocoXCollection }

function TRetPendenciaBlocoXCollection.GetItem(Index: Integer): TRetPendenciaBlocoX;
begin
  Result := TRetPendenciaBlocoX(inherited GetItem(Index));
end;

procedure TRetPendenciaBlocoXCollection.SetItem(Index: Integer;
  Value: TRetPendenciaBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetPendenciaBlocoXCollection.Add: TRetPendenciaBlocoX;
begin
  Result := Self.New;
end;

function TRetPendenciaBlocoXCollection.New: TRetPendenciaBlocoX;
begin
  Result := TRetPendenciaBlocoX.Create;
  Self.Add(Result);
end;

{ TRetConsPendContrEstoqueBlocoXCollection }

function TRetConsPendContrEstoqueBlocoXCollection.GetItem(
  Index: Integer): TRetConsPendContrEstoqueBlocoX;
begin
  Result := TRetConsPendContrEstoqueBlocoX(inherited GetItem(Index));
end;

procedure TRetConsPendContrEstoqueBlocoXCollection.SetItem(Index: Integer;
  Value: TRetConsPendContrEstoqueBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetConsPendContrEstoqueBlocoXCollection.Add: TRetConsPendContrEstoqueBlocoX;
begin
  Result := Self.New;
end;

function TRetConsPendContrEstoqueBlocoXCollection.New: TRetConsPendContrEstoqueBlocoX;
begin
  Result := TRetConsPendContrEstoqueBlocoX.Create;
  Self.Add(Result);
end;

{ TRetECFBlocoXCollection }

function TRetECFBlocoXCollection.GetItem(Index: Integer): TRetECFBlocoX;
begin
  Result := TRetECFBlocoX(inherited GetItem(Index));
end;

procedure TRetECFBlocoXCollection.SetItem(Index: Integer; Value: TRetECFBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetECFBlocoXCollection.Add: TRetECFBlocoX;
begin
  Result := Self.New;
end;

function TRetECFBlocoXCollection.New: TRetECFBlocoX;
begin
  Result := TRetECFBlocoX.Create;
  Self.Add(Result);
end;

{ TRetConsPendContrECF }

constructor TRetECFBlocoX.Create;
begin
  FPendencias := TRetPendenciaBlocoXCollection.Create;
  FAvisos     := TRetAvisoBlocoXCollection.Create;
end;

destructor TRetECFBlocoX.Destroy;
begin
  FPendencias.Free;
  FAvisos.Free;
  inherited Destroy;
end;


{ TRetEstabelecimentoBlocoXCollection }

function TRetEstabelecimentoBlocoXCollection.GetItem(Index: Integer): TRetEstabelecimentoBlocoX;
begin
  Result := TRetEstabelecimentoBlocoX(inherited GetItem(Index));
end;

procedure TRetEstabelecimentoBlocoXCollection.SetItem(Index: Integer;
  Value: TRetEstabelecimentoBlocoX);
begin
  inherited SetItem(Index, Value);
end;

function TRetEstabelecimentoBlocoXCollection.Add: TRetEstabelecimentoBlocoX;
begin
  Result := Self.New;
end;

function TRetEstabelecimentoBlocoXCollection.New: TRetEstabelecimentoBlocoX;
begin
  Result := TRetEstabelecimentoBlocoX.Create;
  Self.Add(Result);
end;

{ TRetPendenciaBlocoX }

constructor TRetPendenciaBlocoX.Create;
begin
  FRecibos := TRetReciboBlocoXCollection.Create;
end;

destructor TRetPendenciaBlocoX.Destroy;
begin
  FRecibos.Free;
  inherited Destroy;
end;

{ TRetConsPendContrReducoesZBlocoX }

constructor TRetConsPendContrReducoesZBlocoX.Create;
begin
  FECFs := TRetECFBlocoXCollection.Create;
end;

destructor TRetConsPendContrReducoesZBlocoX.Destroy;
begin
  FECFs.Free;
  inherited;
end;

{ TRetConsPendContrEstoqueBlocoX }

constructor TRetConsPendContrEstoqueBlocoX.Create;
begin
  FPendencias := TRetPendenciaBlocoXCollection.Create;
  FAvisos     := TRetAvisoBlocoXCollection.Create;
end;

destructor TRetConsPendContrEstoqueBlocoX.Destroy;
begin
  FPendencias.Free;
  FAvisos.Free;
  inherited;
end;

{ TRetConsPendContribuinteBlocoX }

constructor TRetConsPendContribuinteBlocoX.Create;
begin
  inherited;
  FLeitor   := TLeitor.Create;
  FReducaoZ := TRetConsPendContrReducoesZBlocoX.Create;
  FEstoques := TRetConsPendContrEstoqueBlocoXCollection.Create;
end;

destructor TRetConsPendContribuinteBlocoX.Destroy;
begin
  FLeitor.Free;
  FReducaoZ.Free;
  FEstoques.Free;
  inherited;
end;

function TRetConsPendContribuinteBlocoX.LerXml: Boolean;
var
  I, K, J: Integer;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaConsultarPendenciasContribuinte') <> '') then
    begin
      FIE               := Leitor.rCampo(tcStr, 'IE');
      FSituacaoOperCod  := Leitor.rCampo(tcInt, 'SituacaoOperacaoCodigo');
      FSituacaoOperStr  := Leitor.rCampo(tcStr, 'SituacaoOperacaoDescricao');
      FDataObrigacao    := Leitor.rCampo(tcDat, 'DatainicioObrigacao');
      FTransmiteEstoque := StrToBoolDef(Leitor.rCampo(tcEsp, 'TransmiteEstoque'), True);

      // Lendo Reduções Z
      if (Leitor.rExtrai(2, 'ReducoesZ') <> '') then
      begin
        I := 0;
        while (Leitor.rExtrai(3, 'ECF', '', I + 1) <> '') do
        begin
          with ReducaoZ.ECFs.New do
          begin
            FNumeroFabricacao  := Leitor.rCampo(tcStr, 'NumeroFabricacaoEcf');
            FSituacaoPafECFCod := Leitor.rCampo(tcEsp, 'SituacaoPafEcfCodigo');
            FSituacaoPafECFStr := Leitor.rCampo(tcStr, 'SituacaoPafEcfDescricao');
            FQtdPendencias     := Leitor.rCampo(tcInt, 'QuantidadePendencias');
            FQtdAvisos         := Leitor.rCampo(tcInt, 'QuantidadeAvisos');

            // Lendo Pendências
            if (Leitor.rExtrai(4, 'Pendencias') <> '') then
            begin
              J := 0;
              while (Leitor.rExtrai(5, 'Pendencia', '', J + 1) <> '') do
              begin
                with Pendencias.New do
                begin
                  FCodigo     := Leitor.rCampo(tcInt, 'Codigo');
                  FDescricao  := Leitor.rCampo(tcInt, 'Descricao');
                  FQuantidade := Leitor.rCampo(tcInt, 'Quantidade');

                  // Lendo Recibos
                  if (Leitor.rExtrai(6, 'Recibos') <> '') then
                  begin
                    K := 0;
                    while (Leitor.rExtrai(7, 'Recibo', '', K + 1) <> '') do
                    begin
                      with Recibos.New do
                        FRecibo := Leitor.rCampo(tcStr, 'Recibo');
                      Inc(K);
                    end;
                  end;
                end;

                Inc(J);
              end;
            end;

            // Lendo Avisos
            if (Leitor.rExtrai(4, 'Avisos') <> '') then
            begin
              J := 0;
              while (Leitor.rExtrai(5, 'Aviso', '', J + 1) <> '') do
              begin
                with Avisos.New do
                begin
                  FCodigo    := Leitor.rCampo(tcInt, 'Codigo');
                  FDescricao := Leitor.rCampo(tcInt, 'Descricao');
                end;

                Inc(J);
              end;
            end;
          end;

          Inc(I);
        end;
      end;

      // Lendo Estoques
      if (Leitor.rExtrai(2, 'Estoques') <> '') then
      begin
        FEstoques.FQtdPendencias := Leitor.rCampo(tcInt, 'QuantidadePendencias');
        FEstoques.FQtdAvisos     := Leitor.rCampo(tcInt, 'QuantidadeAvisos');

        I := 0;
        while (Leitor.rExtrai(3, 'Estoque', '', I + 1) <> '') do
        begin
          with FEstoques.New do
          begin
            FDataReferencia := (Leitor.rCampo(tcDat, 'DataReferencia'));

            // Lendo Pendências
            if (Leitor.rExtrai(4, 'Pendencias') <> '') then
            begin
              J := 0;
              while (Leitor.rExtrai(5, 'Pendencia', '', J + 1) <> '') do
              begin
                with Pendencias.New do
                begin
                  FCodigo     := Leitor.rCampo(tcInt, 'Codigo');
                  FDescricao  := Leitor.rCampo(tcInt, 'Descricao');
                  FQuantidade := Leitor.rCampo(tcInt, 'Quantidade');

                  // Lendo Recibos
                  if (Leitor.rExtrai(6, 'Recibos') <> '') then
                  begin
                    K := 0;
                    while (Leitor.rExtrai(7, 'Recibo', '', K + 1) <> '') do
                    begin
                      with Recibos.New do
                        FRecibo := Leitor.rCampo(tcStr, 'Recibo');
                      Inc(K);
                    end;
                  end;
                end;

                Inc(J);
              end;
            end;

            // Lendo Avisos
            if (Leitor.rExtrai(4, 'Avisos') <> '') then
            begin
              J := 0;
              while (Leitor.rExtrai(5, 'Aviso', '', J + 1) <> '') do
              begin
                with Avisos.New do
                begin
                  FCodigo    := Leitor.rCampo(tcInt, 'Codigo');
                  FDescricao := Leitor.rCampo(tcInt, 'Descricao');
                end;

                Inc(J);
              end;
            end;
          end;

          Inc(I);
        end;
      end;

      Result := True;
    end;

  except
    Result := False;
  end;
end;

{ TRetConsPendDesenvolvedorPafEcfBlocoX }

constructor TRetConsPendDesenvolvedorPafEcfBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
  FEstabelecimentos := TRetEstabelecimentoBlocoXCollection.Create;
end;

destructor TRetConsPendDesenvolvedorPafEcfBlocoX.Destroy;
begin
  FLeitor.Free;
  FEstabelecimentos.Free;
  inherited;
end;

function TRetConsPendDesenvolvedorPafEcfBlocoX.LerXml: Boolean;
var
  I: Integer;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaConsultarPendenciasDesenvolvedorPafEcf') <> '') then
    begin
      FSituacaoOperCod := Leitor.rCampo(tcInt, 'SituacaoOperacaoCodigo');
      FSituacaoOperStr := Leitor.rCampo(tcStr, 'SituacaoOperacaoDescricao');

      if (Leitor.rExtrai(1, 'Estabelecimentos') <> '') then
      begin
        I := 0;
        while (Leitor.rExtrai(2, 'Estabelecimento', '', I + 1) <> '') do
        begin
          with FEstabelecimentos.New do
          begin
            FIE               := Leitor.rCampo(tcStr, 'IE');
            FNomeEmpresarial  := Leitor.rCampo(tcStr, 'NomeEmpresarial');
            FDataObrigacao    := Leitor.rCampo(tcDat, 'DatainicioObrigacao');
            FTransmiteEstoque := StrToBoolDef(Leitor.rCampo(tcEsp, 'TransmiteEstoque'), True);
            FQtdPendencias    := Leitor.rCampo(tcInt, 'QuantidadePendencias');
            FQtdAvisos        := Leitor.rCampo(tcInt, 'QuantidadeAvisos');
          end;

          Inc(I);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetConsultaArquivoBlocoX }

constructor TRetConsultaArquivoBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
end;

destructor TRetConsultaArquivoBlocoX.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetConsultaArquivoBlocoX.LerXML: Boolean;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'RespostaManutencao') <> '') then
    begin
      FRecibo := Leitor.rCampo(tcStr, 'Recibo');
      FSituacaoProcCod := Leitor.rCampo(tcInt, 'SituacaoProcessamentoCodigo');
      FSituacaoProcStr := Leitor.rCampo(tcStr, 'SituacaoProcessamentoDescricao');
      FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
      Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TRetTransmitirBlocoX }

constructor TRetTransmitirBlocoX.Create;
begin
  inherited;
  FLeitor := TLeitor.Create;
end;

destructor TRetTransmitirBlocoX.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetTransmitirBlocoX.LerXml: Boolean;
begin
  Result := False;
  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'Resposta') <> '') then
    begin
      FRecibo          := Leitor.rCampo(tcStr, 'Recibo');
      FSituacaoProcCod := Leitor.rCampo(tcInt, 'SituacaoProcessamentoCodigo');
      FSituacaoProcStr := Leitor.rCampo(tcStr, 'SituacaoProcessamentoDescricao');
      FMensagem        := Leitor.rCampo(tcStr, 'Mensagem');

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

