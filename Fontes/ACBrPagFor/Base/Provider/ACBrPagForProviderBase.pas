{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrPagForProviderBase;

interface

uses
  SysUtils, Classes,
  ACBrPagForInterface, ACBrPagForClass, ACBrPagForConversao,
  ACBrPagForLerTxt, ACBrPagForGravarTxt,
  ACBrPagForArquivo, ACBrPagForParametros;

type
  TACBrPagForProvider = class(TInterfacedObject, IACBrPagForProvider)
  private
    FConfigGeral: TConfigGeral;

    function GetConfigGeral: TConfigGeral;
  protected
    FAOwner: TComponent;

    function CriarGeradorTxt(const aPagFor: TPagFor): TArquivoWClass; virtual; abstract;
    function CriarLeitorTxt(const aPagFor: TPagFor): TArquivoRClass; virtual; abstract;

    procedure Configuracao; virtual;

    procedure GerarArquivo(const aNomeArq: string); virtual; abstract;

    procedure SalvarTxt(const aNomeArq, aArquivoTxt: string);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function GerarTxt(const aPagFor: TPagFor; var aTxt, aAlerts: string): Boolean; virtual;
    function LerTxt(const aTxt: String; var aPagFor: TPagFor): Boolean; virtual;

    procedure Gerar(const aNomeArq: string); virtual;
    procedure Ler; virtual;

    property ConfigGeral: TConfigGeral read GetConfigGeral;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrDFeException,
  ACBrPagFor, ACBrPagForConfiguracoes;

{ TACBrPagForProvider }

procedure TACBrPagForProvider.Configuracao;
begin
  with TACBrPagFor(FAOwner).Configuracoes.Geral do
  begin
    ConfigGeral.UsarDadosConfig := UsarDadosConfig;

    if UsarDadosConfig then
    begin
      ConfigGeral.Empresa.TipoInscricao := Empresa.TipoInscricao;
      ConfigGeral.Empresa.NumeroInscricao := Empresa.NumeroInscricao;
      ConfigGeral.Empresa.Nome := Empresa.Nome;
      ConfigGeral.Empresa.Convenio := Empresa.Convenio;

      ConfigGeral.Empresa.Conta.AgenciaCodigo := Empresa.Conta.AgenciaCodigo;
      ConfigGeral.Empresa.Conta.AgenciaDV := Empresa.Conta.AgenciaDV;
      ConfigGeral.Empresa.Conta.TipoConta := Empresa.Conta.TipoConta;
      ConfigGeral.Empresa.Conta.ContaNumero := Empresa.Conta.ContaNumero;
      ConfigGeral.Empresa.Conta.ContaDV := Empresa.Conta.ContaDV;
      ConfigGeral.Empresa.Conta.DV := Empresa.Conta.DV;

      ConfigGeral.Empresa.Endereco.Logradouro := Empresa.Endereco.Logradouro;
      ConfigGeral.Empresa.Endereco.Numero := Empresa.Endereco.Numero;
      ConfigGeral.Empresa.Endereco.Complemento := Empresa.Endereco.Complemento;
      ConfigGeral.Empresa.Endereco.Cidade := Empresa.Endereco.Cidade;
      ConfigGeral.Empresa.Endereco.CEP := Empresa.Endereco.CEP;
      ConfigGeral.Empresa.Endereco.Estado := Empresa.Endereco.Estado;
    end;
  end;
end;

constructor TACBrPagForProvider.Create(AOwner: TComponent);
begin
  inherited Create;

  FAOwner := AOwner;
  if not Assigned(FAOwner) then
    raise EACBrDFeException.Create('Componente ACBrPagFor não informado');

  FConfigGeral := TConfigGeral.Create;

  Configuracao;
end;

destructor TACBrPagForProvider.Destroy;
begin
  FConfigGeral.Free;

  inherited Destroy;
end;

function TACBrPagForProvider.GerarTxt(const aPagFor: TPagFor; var aTxt,
  aAlerts: string): Boolean;
var
  AWriter: TArquivoWClass;
begin
  AWriter := CriarGeradorTxt(aPagFor);

  try
    AWriter.Banco := TACBrPagFor(FAOwner).Configuracoes.Geral.Banco;

    Result := AWriter.GerarTxt;

    aTxt := AWriter.ConteudoTxt;
  finally
    AWriter.Destroy;
  end;
end;

function TACBrPagForProvider.GetConfigGeral: TConfigGeral;
begin
  Result := FConfigGeral;
end;

function TACBrPagForProvider.LerTxt(const aTxt: String; var aPagFor: TPagFor): Boolean;
var
  AReader: TArquivoRClass;
begin
  AReader := CriarLeitorTxt(aPagFor);
  AReader.Arquivo := aTxt;

  try
    AReader.Banco := TACBrPagFor(FAOwner).Configuracoes.Geral.Banco;

    Result := AReader.LerTxt;
  finally
    AReader.Destroy;
  end;
end;

procedure TACBrPagForProvider.SalvarTxt(const aNomeArq, aArquivoTxt: string);
var
  NomeArq: string;
  ArquivoGerado: TStringList;
begin
  NomeArq := Trim(aNomeArq);

  if EstaVazio(NomeArq) then
    NomeArq := PathWithDelim(TACBrPagFor(FAOwner).Configuracoes.Arquivos.PathSalvar) +
               '\Cpg'+FormatDateTime('DDMM', now) + '-' +
               BancoToStr(TACBrPagFor(FAOwner).Configuracoes.Geral.Banco) +
               '.seq';

  ArquivoGerado := TStringList.Create;
  try
    ArquivoGerado.Text := aArquivoTxt;

    // remove todas as linhas em branco do arquivo
    RemoveEmptyLines(ArquivoGerado);


    ArquivoGerado.SaveToFile(aNomeArq);
  finally
    ArquivoGerado.Free;
  end;
end;

procedure TACBrPagForProvider.Gerar(const aNomeArq: string);
begin
  GerarArquivo(aNomeArq);
end;

procedure TACBrPagForProvider.Ler;
begin
  // Implementar
end;

end.
