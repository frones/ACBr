{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibResposta;

interface

uses
  SysUtils, Classes, TypInfo,
  ACBrBase,
  ACBrLibConfig;

const
  CSessaoHttpResposta = 'RespostaHttp';

type
  TACBrObjectSerializer = class;

  { TACBrLibRespostaBase }

  TACBrLibRespostaBase = class abstract
  private
    FSessao: String;
    FTipo: TACBrLibRespostaTipo;
    FCodificacao: TACBrLibCodificacao;
    FObjectSerializer: TACBrObjectSerializer;
  protected
    property Tipo: TACBrLibRespostaTipo read FTipo;
    property Codificacao: TACBrLibCodificacao read FCodificacao;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const ACodificacao: TACBrLibCodificacao);
    destructor Destroy; override;
    function Gerar: Ansistring; virtual;

    property Sessao: String read FSessao;
  end;

  { TACBrLibResposta }

  TACBrLibResposta<T: TACBrComponent> = class abstract(TACBrLibRespostaBase)
  public
    procedure Processar(const Control: T); virtual; abstract;
  end;

  { TACBrLibHttpResposta }

  TACBrLibHttpResposta = class(TACBrLibRespostaBase)
  private
    FWebService: string;
    FCodigoHTTP: Integer;
    FMsg: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
  published
    property WebService: String read FWebService write FWebService;
    property CodigoHTTP: Integer read FCodigoHTTP write FCodigoHTTP;
    property Msg: string read FMsg write FMsg;
  end;

  { TACBrLibRespostaEnvio }

  TACBrLibRespostaEnvio = class(TACBrLibRespostaBase)
  private
    FInformacoesArquivo: TACBrObjectList;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    destructor Destroy; override;
  published
    property InformacoesArquivo: TACBrObjectList read FInformacoesArquivo;
  end;

  { TLibImpressaoResposta }
  TLibImpressaoResposta = class(TACBrLibRespostaBase)
  private
    FMsg: string;
  public
    constructor Create(const QtdImpresso: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
  published
    property Msg: string read FMsg write FMsg;
  end;

  { TACBrLibArquivosResposta }

  TACBrLibArquivosResposta = class(TACBrLibRespostaBase)
  private
    FNomeArquivo: String;
    FCaminhoCompleto: String;
  public
  published
    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property CaminhoCompleto: String read FCaminhoCompleto write FCaminhoCompleto;
  end;

  { TACBrObjectSerializerBase }

  TACBrObjectSerializer = class
  private
    FTipo: TACBrLibRespostaTipo;
    FFormato: TACBrLibCodificacao;
  protected
    function GerarXml(Item: TACBrLibRespostaBase): Ansistring; virtual;
    function GerarIni(Item: TACBrLibRespostaBase): Ansistring; virtual;
    function GerarJson(Item: TACBrLibRespostaBase): Ansistring; virtual;

    property Tipo: TACBrLibRespostaTipo read FTipo;
    property Formato: TACBrLibCodificacao read FFormato;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao = codUTF8);

    function Gerar<T: TACBrLibRespostaBase>(const Item: T): Ansistring; overload;
    function Gerar<T: TACBrLibRespostaBase>(const Items: TArray<T>): Ansistring; overload;
  end;

implementation

uses
  StrUtils,
  ACBrUtil.Strings,
  ACBrLibComum,
{$IfDef FPC}
  ACBrObjectSerializerFPC
{$Else}
  ACBrObjectSerializerDelphi
{$EndIf};

{ TACBrLibRespostaEnvio }

constructor TACBrLibRespostaEnvio.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  FInformacoesArquivo := TACBrObjectList.Create(True);
  inherited Create(ASessao, ATipo, AFormato);
end;

destructor TACBrLibRespostaEnvio.Destroy;
begin
  FInformacoesArquivo.Free;
  inherited Destroy;
end;

{ TACBrLibResposta }

constructor TACBrLibRespostaBase.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const ACodificacao: TACBrLibCodificacao);
begin
  inherited Create;
  FSessao := ASessao;
  FTipo := ATipo;
  FCodificacao := ACodificacao;
{$IfDef FPC}
  FObjectSerializer := TACBrObjectSerializerFPC.Create(ATipo, ACodificacao);
{$Else}
  FObjectSerializer := TACBrObjectSerializerDelphi.Create(ATipo, ACodificacao);
{$EndIf}
end;

destructor TACBrLibRespostaBase.Destroy;
begin
  FObjectSerializer.Free;
  inherited Destroy;
end;

function TACBrLibRespostaBase.Gerar: Ansistring;
begin
  Result := ConverterStringNativaParaSaida( FObjectSerializer.Gerar<TACBrLibRespostaBase>(Self), FCodificacao);
end;

{ TACBrLibHttpResposta }

constructor TACBrLibHttpResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoHttpResposta, ATipo, AFormato);
end;

{ TLibImpressaoResposta }

constructor TLibImpressaoResposta.Create(const QtdImpresso: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('Impressao', ATipo, AFormato);
  Msg := Format('%d Documento (s) impresso(s) com sucesso', [QtdImpresso]);
end;

{ TACBrObjectSerializer }

constructor TACBrObjectSerializer.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create;
  FTipo := ATipo;
  FFormato := AFormato;
end;

function TACBrObjectSerializer.Gerar<T>(const Item: T): Ansistring;
begin
  case Tipo of
    resXML: Result := GerarXml(Item);
    resJSON: Result := GerarJson(Item);
    else
      Result := GerarIni(Item);
  end;
end;

function TACBrObjectSerializer.Gerar<T>(const Items: TArray<T>): Ansistring;
Var
  I: Integer;
  Item: TACBrLibRespostaBase;
  LJson : String;
begin
  Result := '';
  For I := 0 to High(Items) do
  begin
    Item := Items[I] as TACBrLibRespostaBase;
    case Tipo of
      resXML: Result := Result + GerarXml(Item);
      resJSON:
        begin
          LJson := ifThen(Result = '', '', Result + ',' );
          Result := LJson + GerarJson(Item);
        end;
    else
      Result := Result + GerarIni(Item);
    end;
  end;

  case Tipo of
    resXML: Result := '<Items>' + Result + '</Items>';
    resJSON: Result := '[' + Result + ']';
  end;
end;

function TACBrObjectSerializer.GerarXml(Item: TACBrLibRespostaBase): Ansistring;
begin
  Result := '';
end;

function TACBrObjectSerializer.GerarIni(Item: TACBrLibRespostaBase): Ansistring;
begin
  Result := '';
end;

function TACBrObjectSerializer.GerarJson(Item: TACBrLibRespostaBase): Ansistring;
begin
  Result := '';
end;

end.


