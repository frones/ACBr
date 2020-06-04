{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrTEFAPIClass;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrTEFComum;

type

  { EACBrTEFAPIException }

  EACBrTEFAPIException = class(Exception)
  public
    constructor CreateAnsiStr(const msg: String);
  end;

  { TACBrTEFAPIResp }

  TACBrTEFAPIResp = class(TACBrTEFResp)
  public
    function VerificarSequenciaInformacao(const Identificacao: Integer): Boolean; virtual;
    procedure GravaInformacao(const Identificacao: Integer; const Informacao: AnsiString);
  end;

  { TACBrTEFAPIClass }

  TACBrTEFAPIClass = class(TComponent)
  private
    procedure ErroAbstract(const NomeProcedure: String);
  protected
    fpOwner: TComponent;

    function GetModeloStr: String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;

    procedure CancelarTransacaoPendente;

    procedure Inicializar; virtual;
    procedure DesInicializar; virtual;

    property ModeloStr: String read GetModeloStr;
  end;

implementation

uses
  ACBrTEFAPI, ACBrUtil;

{ TACBrTEFAPIResp }

function TACBrTEFAPIResp.VerificarSequenciaInformacao(const Identificacao: Integer): Boolean;
begin
  Result := False;
end;

procedure TACBrTEFAPIResp.GravaInformacao(const Identificacao: Integer; const Informacao: AnsiString);
var
  Sequencia: Integer;
  AsString: String;
begin
  if VerificarSequenciaInformacao(Identificacao) then
  begin
    Sequencia := 1;
    while (Trim(LeInformacao(Identificacao, Sequencia).AsString) <> '') do // Já Existe ?
      Inc(Sequencia);
  end
  else
    Sequencia := 0;

  AsString := BinaryStringToString(Informacao);  // Converte #10 para "\x0A"
  fpConteudo.GravaInformacao(Identificacao, Sequencia, AsString);
end;

{ EACBrTEFAPIException }

constructor EACBrTEFAPIException.CreateAnsiStr(const msg: String);
begin
  inherited Create(ACBrStr(msg));
end;

{ TACBrTEFAPIClass }

constructor TACBrTEFAPIClass.Create(AOwner: TComponent);
begin
  if not (AOwner is TACBrTEFAPI) then
    raise EACBrTEFAPIException.CreateAnsiStr(sACBrTEFAPIErrClassCreateException);

  inherited Create(AOwner);

  fpOwner := AOwner;
end;

destructor TACBrTEFAPIClass.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrTEFAPIClass.CancelarTransacaoPendente;
begin
  ErroAbstract('CancelarTransacaoPendente');
end;

procedure TACBrTEFAPIClass.Inicializar;
begin
  {}
end;

procedure TACBrTEFAPIClass.DesInicializar;
begin
  {}
end;

procedure TACBrTEFAPIClass.ErroAbstract(const NomeProcedure: String);
begin
  raise EACBrTEFAPIException.Create( Format(sACBrTEFAPIErrNaoImplementado, [NomeProcedure, ModeloStr]) );
end;

function TACBrTEFAPIClass.GetModeloStr: String;
begin
  Result := 'Não Definido';
end;

end.


case Identificacao of
  141, 142,            // 141 - Data Parcela, 142 - Valor Parcela
  600..607, 611..624:  // Dados do Corresp. Bancário
  begin

