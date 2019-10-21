{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 André Ferreira de Moraes               }
{                                       Daniel Simoes de Almeida               }
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
|* 02/05/2011 Isaque Pinheiro
|*  - Primeira Versao ACBrInStore
******************************************************************************}

unit ACBrInStore;

interface

uses
  SysUtils, Classes, ACBrBase, ACBrUtil;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrPrecoUnitario = procedure(const Codigo: string;
                                 var PrecoUnitario: Double) of object;
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrInStore = class(TACBrComponent)
  private
    fPrefixo: String;
    fPeso: Double;
    fTotal: Double;
    fCodigo: String;
    fDV: String;
    FCodificacao: String;
    fsOnGetPrecoUnitario: TACBrPrecoUnitario;

    procedure SetCodificacao(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ZerarDados;
    procedure Desmembrar(const pCodigoEtiqueta: string);

    property Prefixo: String read fPrefixo;
    property Codigo: String read fCodigo;
    property Peso: Double read fPeso;
    property Total: Double read fTotal;
    property DV: String read fDV;
  published
    property OnGetPrecoUnitario: TACBrPrecoUnitario read fsOnGetPrecoUnitario write fsOnGetPrecoUnitario;
    property Codificacao: String read FCodificacao write SetCodificacao;
  end;

implementation

{ TACBrInStore }

constructor TACBrInStore.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  fPrefixo := '';
end;

destructor TACBrInStore.Destroy;
begin

  inherited;
end;

procedure TACBrInStore.SetCodificacao(const Value: string);
var
pCodigo: Integer;
begin
   FCodificacao := Value;
   // Variáveis de posição
   pCodigo  := Pos('C', FCodificacao);
   // Desmembrar os campos
   // Profixo
   fPrefixo := Copy(FCodificacao, 1, pCodigo -1);
end;

procedure TACBrInStore.ZerarDados;
begin
  fCodigo  := '';
  fDV      := '';
  fPeso    := 0;
  fTotal   := 0;
end;

procedure TACBrInStore.Desmembrar(const pCodigoEtiqueta: string);
var
  // Variáveis de posição
  pCodigo: Integer;
  pTotal: Integer;
  pPeso: Integer;
  // Variáveis de tamanho
  tCodigo: Integer;
  tTotal: Integer;
  tPeso: Integer;
  // Digito verificador
  iFor: Integer;
  fPrecoUnitario: Double;
begin
  if Length(FCodificacao) < 13 then
     raise Exception.Create('Codificação inválida!');

  if Length(pCodigoEtiqueta) < 13 then
     raise Exception.Create('Código EAN13 inválido!');

  if not ACBrUtil.EAN13Valido(pCodigoEtiqueta) then
     raise Exception.Create('Digito verificador do código EAN13 inválido!');

  // Limpa fields
  ZerarDados;
  //
  fPrecoUnitario := 0;

  // Variáveis de posição
  pCodigo := Pos('C', FCodificacao);
  pPeso   := Pos('P', FCodificacao);
  pTotal  := Pos('T', FCodificacao);

  // Variáveis de tamanho
  tCodigo := 0;
  tTotal  := 0;
  tPeso   := 0;

  for iFor := 1 to Length(FCodificacao) do
  begin
    if FCodificacao[iFor] = 'C' then
      Inc(tCodigo)
    else
    if FCodificacao[iFor] = 'P' then
      Inc(tPeso)
    else
    if FCodificacao[iFor] = 'T' then
      Inc(tTotal);
  end;

  // Código
  if pCodigo > 0 then
     fCodigo := Copy(pCodigoEtiqueta, pCodigo, tCodigo);

  // Peso
  if pPeso > 0 then
  begin
    fPeso := StrToCurrDef( Copy(pCodigoEtiqueta, pPeso, tPeso), 0);
    fPeso := fPeso / 1000;
  end;

  // Total
  if pTotal > 0 then
  begin
    fTotal := StrToCurrDef( Copy(pCodigoEtiqueta, pTotal, tTotal), 0);
    fTotal := fTotal / 100;
  end;

  // Caso use somente o peso, poderá ser buscado o preço unitário para achar
  // o valor total
  if Assigned( fsOnGetPrecoUnitario ) then
  begin
     fsOnGetPrecoUnitario( fCodigo, fPrecoUnitario );

     // Se:
     // Valor unitário maior que zero
     // Peso maior que zero
     // Será calculado o preço total
     if (fPrecoUnitario > 0) and (fPeso > 0) then
        fTotal := fPrecoUnitario * fPeso;

     // Se:
     // Valor unitário maior que zero
     // Valor total maior que zero
     // Peso igual a zero
     // Será calculado o peso do produto
     if (fPrecoUnitario > 0) and (fTotal > 0) and (fPeso = 0) then
        fPeso := fTotal / fPrecoUnitario;
  end;
  // Captura digito verificador
  fDV := Copy(pCodigoEtiqueta, Length(pCodigoEtiqueta), 1);
end;

end.
