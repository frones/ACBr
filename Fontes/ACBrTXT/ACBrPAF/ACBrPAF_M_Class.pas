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

unit ACBrPAF_M_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrTXTClass,
     ACBrPAF_M;

type

  { TPAF_M }

  TPAF_M = class(TACBrTXTClass)
  private
    FRegistroM2: TRegistroM2List;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros;

    procedure WriteRegistroM2;

    property RegistroM2: TRegistroM2List read FRegistroM2 write FRegistroM2;
  end;

implementation

{ TPAF_M }
constructor TPAF_M.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TPAF_M.CriaRegistros;
begin
  FRegistroM2 := TRegistroM2List.Create;
end;

destructor TPAF_M.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TPAF_M.LiberaRegistros;
begin
  FRegistroM2.Free;
end;

procedure TPAF_M.LimpaRegistros;
begin
  //Limpa os Registros
  LiberaRegistros;
  //Recriar os Registros Limpos
  CriaRegistros;
end;

function OrdenarM2(const ARegistro1, ARegistro2: Pointer): Integer;
var
  Reg1, Reg2: String;
begin
  Reg1 :=
    TRegistroM2(ARegistro1).CNPJ +
    Format('%-8s', [TRegistroM2(ARegistro1).ID_LINHA]) +
    FormatDateTime('yyyymmddhhmmss', TRegistroM2(ARegistro1).DT_VIA) +
    Format('%-20s', [TRegistroM2(ARegistro1).NUM_FAB]) +
    Format('%6.6d', [TRegistroM2(ARegistro1).CCF]) +
    Format('%6.6d', [TRegistroM2(ARegistro1).COO]);

  Reg2 :=
    TRegistroM2(ARegistro2).CNPJ +
    Format('%-8s', [TRegistroM2(ARegistro2).ID_LINHA]) +
    FormatDateTime('yyyymmddhhmmss', TRegistroM2(ARegistro2).DT_VIA) +
    Format('%-20s', [TRegistroM2(ARegistro2).NUM_FAB]) +
    Format('%6.6d', [TRegistroM2(ARegistro2).CCF]) +
    Format('%6.6d', [TRegistroM2(ARegistro2).COO]);

  Result := AnsiCompareText(Reg1, Reg2);
end;

procedure TPAF_M.WriteRegistroM2;
var
  intFor: integer;
begin
  if Assigned(FRegistroM2) then
  begin
    FRegistroM2.Sort(@OrdenarM2);

    for intFor := 0 to FRegistroM2.Count - 1 do
    begin
      with FRegistroM2.Items[intFor] do
      begin
        Add( LFill('M2') +
             LFill(CNPJ, 14) +
             RFill(IE, 14) +
             RFill(IM, 14) +
             RFill(NUM_FAB, 20) +
             RFill(MF_ADICIONAL, 1) +
             RFill(TIPO_ECF, 7) +
             RFill(MARCA_ECF, 20) +
             RFill(MODELO_ECF, 20) +
             LFill(NUM_USU, 2) +
             LFill(CCF, 6) +
             LFill(COO, 6) +
             LFill(DT_EMI, 'yyyymmddhhmmss') +
             LFill(COD_MOD, 2) +
             LFill(COD_CAT, 2) +
             RFill(ID_LINHA, 8) +
             RFill(COD_ORIG, 20) +
             RFill(COD_DEST, 20) +
             LFill(COD_TSER, 2) +
             LFill(DT_VIA, 'yyyymmddhhmmss') +
             LFill(TIP_VIA, 2) +
             LFill(POLTRONA, 7) +
             RFill(PLATAFORMA,15) +
             LFill(COD_DESC, 2) +
             LFill(VL_TARIFA, 8, 2) +
             LFill(ALIQ, 4, 2) +
             LFill(VL_PEDAGIO, 8, 2) +
             LFill(VL_TAXA, 8, 2) +
             LFill(VL_TOTAL, 8, 2) +
             LFill(FORM_PAG, 2) +
             LFill(VL_PAGO, 8, 2) +
             RFill(NOME_PAS, 50) +
             RFill(NDOC_PAS, 20) +
             RFill(SAC, 10) +
             RFill(AGENCIA, 30));
      end;
    end;
  end;

end;

end.
 
