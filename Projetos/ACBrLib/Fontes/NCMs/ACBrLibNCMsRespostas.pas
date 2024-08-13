{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elton Barbosa                                   }
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

unit ACBrLibNCMsRespostas;

interface

uses
  SysUtils, Classes, contnrs,
  ACBrLibResposta, ACBrLibConfig, ACBrNCMs;

type

  { TNCMsRespostaSimples }
  TNCMsRespostaSimples = class(TObject)
  private
    FStringListNCMs: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Processar(const UmNCM: TACBrNCMs);
    function Gerar: Ansistring;
  end;

  { TNCMsRespostaExtendido }
  TNCMsRespostaExtendido = class(TACBrLibRespostaBase)
  private
    FQtd: Integer;
    FItems: TACBrNCMsList;
  public
    procedure Processar(const umACBrNCMs: TACBrNCMs);
  published
    property Quantidade: Integer read FQtd write FQtd;
    property Items: TACBrNCMsList read FItems;
  end;

  { TNCMsRespostaFactory }

  TNCMsRespostaFactory = class(TObject)
  private
    FUsarRespostaExtendida: Boolean;
    FRespostaSimples: TNCMsRespostaSimples;
    FRespostaExtendido: TNCMsRespostaExtendido;
  public
    constructor Create(const UsarRespostaExtendida: Boolean; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);

    procedure Processar(const UmNCM: TACBrNCMs);
    function Gerar: Ansistring;
  end;

implementation

Uses
  ACBrLibNCMsConsts;

{ TNCMsRespostaFactory }

constructor TNCMsRespostaFactory.Create(const UsarRespostaExtendida: Boolean; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create;
  FUsarRespostaExtendida := UsarRespostaExtendida;
  if FUsarRespostaExtendida then
    FRespostaExtendido := TNCMsRespostaExtendido.Create(CSessaoNCMs, ATipo, AFormato)
  else
    FRespostaSimples := TNCMsRespostaSimples.Create;
end;

procedure TNCMsRespostaFactory.Processar(const UmNCM: TACBrNCMs);
begin
  if FUsarRespostaExtendida then
    FRespostaExtendido.Processar(UmNCM)
  else
    FRespostaSimples.Processar(UmNCM);
end;

function TNCMsRespostaFactory.Gerar: Ansistring;
begin
  if FUsarRespostaExtendida then
    Result := FRespostaExtendido.Gerar
  else
    Result := FRespostaSimples.Gerar;
end;

constructor TNCMsRespostaSimples.Create;
begin
  inherited;
  FStringListNCMs := TStringList.Create;
end;

destructor TNCMsRespostaSimples.Destroy;
begin
  FStringListNCMs.Free;
  inherited Destroy;
end;

{ TNCMsRespostaSimples }
procedure TNCMsRespostaSimples.Processar(const UmNCM: TACBrNCMs);
begin
  FStringListNCMs.Clear;
  UmNCM.NCMsFiltrados.SaveToStringList(FStringListNCMs, '|');
end;

function TNCMsRespostaSimples.Gerar: Ansistring;
begin
  Result := FStringListNCMs.Text;
end;

{ TNCMsRespostaExtendido }
procedure TNCMsRespostaExtendido.Processar(const umACBrNCMs: TACBrNCMs);
begin
  FQtd := umACBrNCMs.NCMsFiltrados.Count;
  FItems := umACBrNCMs.NCMsFiltrados;
end;

end.
