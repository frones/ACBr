{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit pgnreRetConsConfigUF;

interface

uses
  SysUtils, Classes,
  ACBrUtil,
  pcnAuxiliar, pcnConversao, pcnLeitor, pgnreRetReceita;

type

  TTConfigUf = class(TObject)
  private
    FAmbiente: TpcnTipoAmbiente;
    FLeitor: TLeitor;
    FUf: string;
    Fcodigo: Integer;
    Fdescricao: string;
    FexigeUfFavorecida: string;
    FexigeReceita: string;
    FInfReceita: TRetReceita;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;

    property Ambiente: TpcnTipoAmbiente read FAmbiente write FAmbiente;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Uf: string read FUf write FUf;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: string read Fdescricao write Fdescricao;
    property exigeUfFavorecida: string read FexigeUfFavorecida write FexigeUfFavorecida;
    property exigeReceita: string read FexigeReceita write FexigeReceita;
    property InfReceita: TRetReceita read FInfReceita write FInfReceita;
  end;

implementation

{ TTConfigUf }

constructor TTConfigUf.Create;
begin
  FLeitor := TLeitor.Create;
  FInfReceita := TRetReceita.Create;
end;

destructor TTConfigUf.Destroy;
begin
  FLeitor.Free;
  FInfReceita.Free;

  inherited;
end;

function TTConfigUf.LerXml: boolean;
var
  ok: Boolean;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    //Faltou o namespace ns1
    if Leitor.rExtrai(1, 'ns1:TConfigUf') <> '' then
    begin
      FAmbiente          := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'ns1:ambiente'));
      FUf                := Leitor.rCampo(tcStr, 'ns1:Uf');
      Fcodigo            := Leitor.rCampo(tcInt, 'ns1:codigo');
      Fdescricao         := Leitor.rCampo(tcStr, 'ns1:descricao');
      FexigeUfFavorecida := SeparaDados(Leitor.Grupo, 'ns1:exigeUfFavorecida');
      FexigeReceita      := SeparaDados(Leitor.Grupo, 'ns1:exigeReceita');

      if SameText(FexigeReceita, 'S') then
      begin
        if Leitor.rExtrai(2, 'ns1:receitas') <> '' then
        begin
          InfReceita.Leitor.Arquivo := Leitor.Grupo;
          InfReceita.LerXml;
        end;
      end;

      Result := True;
    end;
  except
    Result := false;
  end;
end;

end.
