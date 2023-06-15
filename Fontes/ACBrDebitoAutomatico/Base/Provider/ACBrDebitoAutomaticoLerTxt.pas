{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit ACBrDebitoAutomaticoLerTxt;

interface

uses
  SysUtils, Classes,
  ACBrDebitoAutomaticoInterface, ACBrDebitoAutomaticoClass, ACBrDebitoAutomaticoConversao;

type
  { TArquivoRClass }

  TArquivoRClass = class
  private
    FDebitoAutomatico: TDebitoAutomatico;
    FBanco: TBanco;
    FArquivo: string;
  protected
    FpAOwner: IACBrDebitoAutomaticoProvider;

    procedure Configuracao; virtual;

    function LerCampo(const Linha: string; Inicio, Tamanho: Integer;
      Tipo: TTipoCampo): Variant;
  public
    constructor Create(AOwner: IACBrDebitoAutomaticoProvider);

    function LerTxt: Boolean; virtual;

    property DebitoAutomatico: TDebitoAutomatico read FDebitoAutomatico  write FDebitoAutomatico;
    property Banco: TBanco   read FBanco   write FBanco;
    property Arquivo: string read FArquivo write FArquivo;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrDFeException;

{ TArquivoRClass }

procedure TArquivoRClass.Configuracao;
begin
  // Nada implementado
end;

constructor TArquivoRClass.Create(AOwner: IACBrDebitoAutomaticoProvider);
begin
  FpAOwner := AOwner;
end;

function TArquivoRClass.LerCampo(const Linha: string; Inicio, Tamanho: Integer;
  Tipo: TTipoCampo): Variant;
var
  ConteudoCampo: string;
  iDecimais: Integer;
begin
  ConteudoCampo := Copy(Trim(Linha), Inicio, Tamanho);
  ConteudoCampo := Trim(ConteudoCampo);

  case Tipo of
    tcStr:
      result := ConteudoCampo;

    tcDat:
      begin
        if length(ConteudoCampo) > 0 then
        begin
          if ConteudoCampo <> '00000000' then
          begin
            ConteudoCampo := Copy(ConteudoCampo, 1, 2) + '/' +
                             Copy(ConteudoCampo, 3, 2) + '/' +
                             Copy(ConteudoCampo, 5, 4);
            result := EncodeDataHora(ConteudoCampo, 'DD/MM/YYYY');
          end
          else
            Result := 0;
        end
        else
          result := 0;
      end;

    tcDatISO:
      begin
        if length(ConteudoCampo) > 0 then
        begin
          if ConteudoCampo <> '00000000' then
            result := EncodeDataHora(ConteudoCampo, 'YYYY/MM/DD')
          else
            Result := 0;
        end
        else
          result := 0;
      end;

    tcHor:
      begin
        if length(ConteudoCampo) > 0 then
          result := EncodeTime(StrToIntDef(copy(ConteudoCampo, 1, 2), 0),
                               StrToIntDef(copy(ConteudoCampo, 3, 2), 0),
                               StrToIntDef(copy(ConteudoCampo, 5, 2), 0), 0)
        else
          result := 0;
      end;

    tcDe2, tcDe5, tcDe8:
      begin
        case Tipo of
          tcDe2: iDecimais := 2;
          tcDe5: iDecimais := 5;
          tcDe8: iDecimais := 8;
        else
          iDecimais := 2;
        end;

        Result := StringDecimalToFloat(ConteudoCampo, iDecimais);
      end;

    tcInt:
      begin
        if length(ConteudoCampo) > 0 then
          result := StrToIntDef(OnlyNumber(ConteudoCampo), 0)
        else
          result := 0;
      end;

    tcInt64:
      begin
        if length(ConteudoCampo) > 0 then
          result := StrToInt64Def(OnlyNumber(ConteudoCampo), 0)
        else
          result := 0;
      end;
  else
    raise Exception.Create('Campo <' + Linha + '> com conteúdo inválido. ' +
                           ConteudoCampo);
  end;
end;

function TArquivoRClass.LerTxt: Boolean;
begin
  Result := False;
  raise EACBrDFeException.Create(ClassName + '.LerTxt, não implementado');
end;

end.
