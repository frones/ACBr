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

{$I ACBr.inc}

unit ACBrPAF_G_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_G;

type

  { TPAF_G }

  TPAF_G = class(TACBrTXTClass)
  private
    FRegistroG2: TRegistroG2List;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros;

    procedure WriteRegistroG2;

    property RegistroG2: TRegistroG2List read FRegistroG2 write FRegistroG2;
  end;

implementation

{ TPAF_G }
constructor TPAF_G.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_G.CriaRegistros;
begin
  FRegistroG2 := TRegistroG2List.Create;
end;

destructor TPAF_G.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_G.LiberaRegistros;
begin
  FRegistroG2.Free;
end;

procedure TPAF_G.LimpaRegistros;
begin
  //Limpa os Registros
  LiberaRegistros;
  //Recriar os Registros Limpos
  CriaRegistros;
end;

function OrdenarG2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 := FormatDateTime('YYYYMMDD', TRegistroG2(ARegistro1).DT);

  Reg2 := FormatDateTime('YYYYMMDD', TRegistroG2(ARegistro2).DT);

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_G.WriteRegistroG2;
var
  intFor: integer;
begin
  if Assigned(FRegistroG2) then
  begin
    FRegistroG2.Sort(@OrdenarG2);

    for intFor := 0 to FRegistroG2.Count - 1 do
    begin
      with FRegistroG2.Items[intFor] do
      begin
        Add( LFill('G2') +
             LFill(CNPJ, 14) +
             RFill(NUM_FAB, 20) +
             RFill(MF_ADICIONAL, 1) +
             RFill(TIPO_ECF, 7) +
             RFill(MARCA_ECF, 20) +
             RFill(MODELO_ECF, 20) +
             LFill(NUM_CAB, 2) +
             LFill(DT, 'yyyymmdd') +
             LFill(COO_INI, 6) +
             LFill(COO_FIN, 6) +
             LFill(CCF_INI, 6) +
             LFill(CCF_FIN, 6) +
             LFill(VL_2EIX_SIMPLES, 9, 2) +
             LFill(VL_2EIX_SIMPLES_MOTO, 9, 2) +
             LFill(VL_2EIX_DUPLA, 9, 2) +
             LFill(VL_3EIX_SIMPLES, 9, 2) +
             LFill(VL_3EIX_DUPLA, 9, 2) +
             LFill(VL_4EIX_SIMPLES, 9, 2) +
             LFill(VL_4EIX_DUPLA, 9, 2) +
             LFill(VL_5EIX_DUPLA, 9, 2) +
             LFill(VL_6EIX_DUPLA, 9, 2) +
             LFill(VL_OUTROS, 9, 2) +
             LFill(VL_TOTAL_DIA, 10, 2) +
             LFill(QTDE_VEIC_ISENTO, 6) +
             RFill(LOCALIZACAO, 250));
      end;
    end;
  end;

end;

end.
 
