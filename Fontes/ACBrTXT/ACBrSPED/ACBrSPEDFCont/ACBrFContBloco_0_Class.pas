{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
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
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrFContBloco_0_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrFContBloco_0;

type
  /// TBLOCO_0 - Abertura, Identificação e Referências
  TBloco_0 = class(TACBrSPED)
  private
    FRegistro0000: TRegistro0000;      /// BLOCO 0 - Registro0000
  protected
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy

    procedure LimpaRegistros; override;

    function WriteRegistro0000: String;

    property Registro0000: TRegistro0000     read FRegistro0000 write FRegistro0000;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_0 }

constructor TBloco_0.Create;
begin
  inherited Create;
  FRegistro0000 := TRegistro0000.Create;
end;

destructor TBloco_0.Destroy;
begin
  FRegistro0000.Free;

  inherited;
end;

procedure TBloco_0.LimpaRegistros;
begin

end;

function TBloco_0.WriteRegistro0000: String;
begin
  Result := '';

  if Assigned(FRegistro0000) then
  begin
     with FRegistro0000 do
     begin
       Check(NOME <> '', '(0-0000) O nome empresarial é obrigatório!');
       Check(funChecaCNPJ(CNPJ), '(0-0000) O CNPJ "%s" digitado é inválido!', [CNPJ]);
       Check(funChecaUF(UF), '(0-0000) A UF "%s" digitada é inválido!', [UF]);
       Check(funChecaIE(IE, UF), '(0-0000) A inscrição estadual "%s" digitada é inválida!', [IE]);
       Check(funChecaMUN(StrToInt(COD_MUN)), '(0-0000) O código do município "%s" digitado é inválido!', [COD_MUN]);
       Check((((IND_SIT_ESP >= '0') and (IND_SIT_ESP <= '4')) or (IND_SIT_ESP = '')), '(0-0000) O indicador "%s" de situação especial, deve ser vazio ou informado o número 0 ou 1 ou 2 ou 3 ou 4!', [IND_SIT_ESP]);
       Check(((IND_SIT_INI_PER >= '0') and (IND_SIT_INI_PER <= '3')), '(0-0000) O indicador "%s" de início do período, deve ser informado o número 0 ou 1 ou 2 ou 3!', [IND_SIT_INI_PER]);
       ///
       Result := LFill('0000') +
                 LFill('LALU') +
                 LFill(DT_INI) +
                 LFill(DT_FIN) +
                 LFill(NOME) +
                 LFill(CNPJ) +
                 LFill(UF) +
                 LFill(IE) +
                 LFill(COD_MUN, 7) +
                 LFill(IM) +
                 LFill(IND_SIT_ESP) +
                 LFill(IND_SIT_INI_PER, 1) +
                 Delimitador +
                 #13#10;
       ///
     end;
  end;
end;

end.
