{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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
unit ACBrPAF_U_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass, ACBrTXTUtils,
     ACBrPAF_U;

type

  { TPAF_U }

  TPAF_U = class(TACBrTXTClass)
  private
    FRegistroU1: TRegistroU1;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override; 
    procedure LimpaRegistros;
    
    procedure WriteRegistroU1;

    property RegistroU1: TRegistroU1 read FRegistroU1 write FRegistroU1;
  end;
  
implementation

{ TPAF_U }
constructor TPAF_U.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_U.CriaRegistros;
begin
  FRegistroU1 := TRegistroU1.Create;
end;

destructor TPAF_U.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_U.LiberaRegistros;
begin
  FRegistroU1.Free;
end;

procedure TPAF_U.LimpaRegistros;
begin
  //Limpa os Registros
  LiberaRegistros;
  //Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TPAF_U.WriteRegistroU1;
begin
  if Assigned(FRegistroU1) then
  begin
    with FRegistroU1 do
    begin
      Check(funChecaCNPJ(CNPJ), '(U1) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);

      Add(LFill('U1') +
          LFill(CNPJ, 14) +
          RFill(IE, 14) +
          RFill(IM, 14) +
          RFill(RAZAOSOCIAL, 50, ifThen(not InclusaoExclusao, ' ', '?')));
    end;
  end;
end;

end.
