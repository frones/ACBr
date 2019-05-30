{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Macgayver Armini Apolonio            }
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
|* 23/02/2015: Macgayver Armini Apolonio
|*  - Criação
|* 30/04/2019: Rodrigo Coelho | Bunny Soft - Tratamento de exceção
|*  - Verificação se o Índique que se está tentando ler não é maior que a quantidade
|*  de colunas disponíveis no arquivo que está sendo importado. Isso pode acontecer
|*  quando tentamos importar arquivos de SPED mais antigos que possuem menos colunas
|*  que a definição atual
*******************************************************************************}
unit ACBrEPCBase;

interface

uses
  Classes,
  SysUtils,
  Variants,
  ACBrSpedPisCofins;

type

  // Permite interceptar os valores inseridos pela ACBR.
  TACBrSpedPCImportarGetColumn = procedure(var Coluna: string; const ColunaI: integer) of Object;

  TACBrSpedPCImportar_Base = class
  private
    FAntesInserirValor: TACBrSpedPCImportarGetColumn;
  protected
    Indice: integer;
    Delimitador: TStrings;
    FACBrSPEDPisCofins: TACBrSPEDPisCofins;

    function Head: string;
    function Valor: string; // Procedimento Base para os outros valores
    function ValorI: integer;
    function ValorF: Currency;
    function ValorFV: Variant;
    function ValorD: TDateTime;

  public
    constructor Create;

    procedure AnalisaRegistro(const inDelimitador: TStrings); virtual;

    property ACBrSpedPisCofins: TACBrSPEDPisCofins read FACBrSPEDPisCofins write FACBrSPEDPisCofins;
    property AntesInserirValor: TACBrSpedPCImportarGetColumn read FAntesInserirValor write FAntesInserirValor;
  end;

implementation

{ TBaseIndice }

procedure TACBrSpedPCImportar_Base.AnalisaRegistro(const inDelimitador: TStrings);
begin
  Delimitador := inDelimitador;
end;

constructor TACBrSpedPCImportar_Base.Create;
begin
  Indice := 1;
end;

function TACBrSpedPCImportar_Base.Head: string;
begin
  Result := Delimitador[1];
end;

function TACBrSpedPCImportar_Base.Valor: string;
var
  vValor: string;
begin
  Indice := Indice + 1;
  // Verificar se Índice a ser lido não é maior que a quantidade de colunas disponíveis no arquivo
  if (Indice <= Delimitador.Count - 1) then
    vValor := Delimitador[Indice]
  else
    vValor := '';

  if Assigned(FAntesInserirValor) then
    FAntesInserirValor(vValor, Indice);

  Result := vValor;
end;

function TACBrSpedPCImportar_Base.ValorD: TDateTime;
var
  S: string;
begin
  S := Valor;
  if S <> EmptyStr then
    Result := EncodeDate(StrToInt(Copy(S, 5, 4)), StrToInt(Copy(S, 3, 2)),StrToInt(Copy(S, 1, 2)))
  else
    Result := 0;
end;

function TACBrSpedPCImportar_Base.ValorI: integer;
var
  S: string;
begin
  S := Valor;
  if S <> EmptyStr then
    Result := StrToInt(S)
  else
    Result := 0;
end;

function TACBrSpedPCImportar_Base.ValorF: Currency;
var
  S: string;
begin
  S := Valor;
  if S <> EmptyStr then
    Result := StrToFloat(S)
  else
    Result := 0;
end;

function TACBrSpedPCImportar_Base.ValorFV: Variant;
var
  S: string;
begin
  S := Valor;
  if S = EmptyStr then
    Result := Null
  else
    Result := StrToFloat(S);
end;

end.
