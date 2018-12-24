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
*******************************************************************************}

unit ACBrFContBloco_M_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrFContBloco_M;

type
  /// TBloco_M - Informações Fiscais
  TBloco_M = class(TACBrSPED)
  private
    FRegistroM001: TRegistroM001;      /// BLOCO M - RegistroM001
    FRegistroM020: TRegistroM020;      /// BLOCO M - RegistroM020
    FRegistroM025: TRegistroM025List;  /// BLOCO M - Lista de RegistroM025
    FRegistroM030: TRegistroM030List;  /// BLOCO M - Lista de RegistroM030
    FRegistroM990: TRegistroM990;      /// BLOCO M - FRegistroM990

    FRegistroM155Count: Integer;
    FRegistroM355Count: Integer;

    function WriteRegistroM155(RegM030: TRegistroM030): String;
    function WriteRegistroM355(RegM030: TRegistroM030): String;

  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function WriteRegistroM001: String;
    function WriteRegistroM020: String;
    function WriteRegistroM025: String;
    function WriteRegistroM030: String;
    function WriteRegistroM990: String;

    property RegistroM001: TRegistroM001     read fRegistroM001 write fRegistroM001;
    property RegistroM020: TRegistroM020     read fRegistroM020 write fRegistroM020;
    property RegistroM025: TRegistroM025List read fRegistroM025 write fRegistroM025;
    property RegistroM030: TRegistroM030List read fRegistroM030 write fRegistroM030;
    property RegistroM990: TRegistroM990     read fRegistroM990 write fRegistroM990;

    property RegistroM155Count: Integer read FRegistroM155Count write FRegistroM155Count;
    property RegistroM355Count: Integer read FRegistroM355Count write FRegistroM355Count;
  end;

implementation

uses ACBrTXTClass;

{ TBloco_M }

constructor TBloco_M.Create;
begin
  inherited;
  FRegistroM001 := TRegistroM001.Create;
  FRegistroM020 := TRegistroM020.Create;
  FRegistroM025 := TRegistroM025List.Create;
  FRegistroM030 := TRegistroM030List.Create;
  FRegistroM990 := TRegistroM990.Create;

  FRegistroM155Count := 0;
  FRegistroM355Count := 0;

  FRegistroM990.QTD_LIN_M := 0;
end;

destructor TBloco_M.Destroy;
begin
  FRegistroM001.Free;
  FRegistroM020.Free;
  FRegistroM025.Free;
  FRegistroM030.Free;
  FRegistroM990.Free;
  inherited;
end;

procedure TBloco_M.LimpaRegistros;
begin
  FRegistroM025.Clear;
  FRegistroM030.Clear;

  FRegistroM990.QTD_LIN_M := 0;
end;

function TBloco_M.WriteRegistroM001: String;
begin
  Result := '';

  if Assigned(FRegistroM001) then
  begin
     with FRegistroM001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(M-M001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('M001') +
                 LFill(IND_DAD, 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroM990.QTD_LIN_M := FRegistroM990.QTD_LIN_M + 1;
     end;
  end;
end;

function TBloco_M.WriteRegistroM020: String;
begin
  Result := '';

  if Assigned(FRegistroM020) then
  begin
     with FRegistroM020 do
     begin
       Check(((TIPO_ESCRIT = '0') or (TIPO_ESCRIT = '1')) , '(M-M020) Tipo de escrituração "%s", deve ser 0-Original or 1-Retificadora',[TIPO_ESCRIT]);
       Check(((QUALI_PJ = '00') or (QUALI_PJ = '10') or (QUALI_PJ = '20')) , '(M-M020) Qualificação da PJ "%s", deve ser 00, 10 ou 20',[QUALI_PJ]);
       Check( ((TIPO_ESCRIT = '0') and (length(NRO_REC_ANTERIOR) = 0)) or (TIPO_ESCRIT = '1'), '(M-M020) Número do recibo anterior "%s" não pode ser preenchido para o tipo de escrituração 0-Original!',[NRO_REC_ANTERIOR]);
       Check(length(NRO_REC_ANTERIOR) <= 41, '(M-M020) Número do recibo anterior "%s" não pode possuir mais de 41 caracteres!',[NRO_REC_ANTERIOR]);
       ///
       Result := LFill('M020') +
                 LFill(QUALI_PJ, 2) +
                 LFill(TIPO_ESCRIT) +
                 LFill(NRO_REC_ANTERIOR) +
                 LFill(ID_ESCR_PER_ANT) +
                 LFill(SIT_SLD_PER_ANT) +
                 LFill(IND_LCTO_INI_SLD) +
                 LFill(FORM_APUR, 1) +
                 LFill(FORM_TRIBUT) +
                 LFill(TRIM_LUC_ARB) +
                 LFill(FORM_TRIB_TRI) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroM990.QTD_LIN_M := FRegistroM990.QTD_LIN_M + 1;
     end;
  end;
end;


function TBloco_M.WriteRegistroM025: String;
var
intFor: integer;
strRegistroM025: String;
begin
  strRegistroM025 := '';

  if Assigned(FRegistroM025) then
  begin
     for intFor := 0 to FRegistroM025.Count - 1 do
     begin
        with FRegistroM025.Items[intFor] do
        begin
           ///
           Check(((IND_DC_FIN_FC = 'D') or (IND_DC_FIN_FC = 'C')) , '(M-M025) Indicador da situação do saldo fiscal final "%s", deve ser D-Devedor ou C-Credor',[IND_DC_FIN_FC]);
           Check(((IND_DC_FIN_SOC = 'D') or (IND_DC_FIN_SOC = 'C')) , '(M-M025) Indicador da situação do saldo Societário final "%s", deve ser D-Devedor ou C-Credor',[IND_DC_FIN_SOC]);
           ///
           strRegistroM025 :=  strRegistroM025 + LFill('M025') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_SLD_FIN_FC, 19,2) +
                                                 LFill(IND_DC_FIN_FC) +
                                                 LFill(VL_SLD_FIN_SOC, 19,2) +
                                                 LFill(IND_DC_FIN_SOC) +
                                                 Delimitador +
                                                 #13#10;
        end;
        FRegistroM990.QTD_LIN_M := FRegistroM990.QTD_LIN_M + 1;
     end;
  end;
  Result := strRegistroM025;
end;


function TBloco_M.WriteRegistroM030: String;
var
intFor: integer;
strRegistroM030: String;
begin
  strRegistroM030 := '';

  if Assigned(FRegistroM030) then
  begin
     for intFor := 0 to FRegistroM030.Count - 1 do
     begin
        with FRegistroM030.Items[intFor] do
        begin
           ///
           Check(((IND_PER = 'A00') or (IND_PER = 'T01') or (IND_PER = 'T02') or (IND_PER = 'T03') or (IND_PER = 'T04')) , '(M-M030) Período de Apuração "%s", deve ser A00, T01, T02, T03 ou T04.',[IND_PER]);
           Check(((IND_LUC_LIQ = 'D') or (IND_LUC_LIQ = 'C')) , '(M-M030) Situação do Resultado do Período "%s", deve ser D-Prejuízo ou C-Lucro',[IND_LUC_LIQ]);
           ///
           strRegistroM030 :=  strRegistroM030 + LFill('M030') +
                                                 LFill(IND_PER) +
                                                 LFill(VL_LUC_LIQ, 19,2) +
                                                 LFill(IND_LUC_LIQ) +
                                                 Delimitador +
                                                 #13#10;
        end;

        // Registros Filhos
        strRegistroM030 := strRegistroM030 +
                           WriteRegistroM155(FRegistroM030.Items[intFor] );

        strRegistroM030 := strRegistroM030 +
                           WriteRegistroM355(FRegistroM030.Items[intFor] );

        FRegistroM990.QTD_LIN_M := FRegistroM990.QTD_LIN_M + 1;
     end;
  end;
  Result := strRegistroM030;
end;

function TBloco_M.WriteRegistroM155(RegM030: TRegistroM030): String;
var
intFor: integer;
strRegistroM155: String;
begin
  strRegistroM155 := '';

  if Assigned(RegM030.RegistroM155) then
  begin
     for intFor := 0 to RegM030.RegistroM155.Count - 1 do
     begin
        with RegM030.RegistroM155.Items[intFor] do
        begin
           ///
           strRegistroM155 :=  strRegistroM155 + LFill('M155') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_SLD_INI_SOC_ANT, 19, 2) +
                                                 LFill(IND_DC_INI_SOC_ANT) +
                                                 LFill(VL_IS_DEB, 19, 2) +
                                                 LFill(VL_IS_CRED, 19, 2) +
                                                 LFill(VL_SLD_INI_SOC, 19, 2) +
                                                 LFill(IND_DC_INI_SOC) +
                                                 LFill(VL_SLD_INI_FC_ANT, 19, 2) +
                                                 LFill(IND_DC_INI_FC_ANT) +
                                                 LFill(VL_IF_DEB, 19, 2) +
                                                 LFill(VL_IF_CRED, 19, 2) +
                                                 LFill(VL_SLD_INI_FC, 19, 2) +
                                                 LFill(IND_DC_INI_FC) +
                                                 LFill(VL_DEB_CONTABIL, 19, 2) +
                                                 LFill(VL_CRED_CONTABIL, 19, 2) +
                                                 LFill(VL_DEB_FCONT_E, 19, 2) +
                                                 LFill(VL_CRED_FCONT_E, 19, 2) +
                                                 LFill(VL_DEB_FCONT_I, 19, 2) +
                                                 LFill(VL_CRED_FCONT_I, 19, 2) +
                                                 LFill(VL_TR_DEB, 19, 2) +
                                                 LFill(VL_TR_CRED, 19, 2) +
                                                 LFill(VL_TF_DEB, 19, 2) +
                                                 LFill(VL_TF_CRED, 19, 2) +
                                                 LFill(VL_TS_DEB, 19, 2) +
                                                 LFill(VL_TS_CRED, 19, 2) +
                                                 LFill(VL_EF_DEB, 19, 2) +
                                                 LFill(VL_EF_CRED, 19, 2) +
                                                 LFill(VL_SLD_FIN_FC, 19, 2) +
                                                 LFill(IND_DC_FIN_FC) +
                                                 LFill(VL_SLD_FIN_SOC, 19, 2) +
                                                 LFill(IND_DC_FIN_SOC) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroM990.QTD_LIN_M := FRegistroM990.QTD_LIN_M + 1;
     end;
     FRegistroM155Count := FRegistroM155Count + RegM030.RegistroM155.Count;
  end;
  Result := strRegistroM155;
end;

function TBloco_M.WriteRegistroM355(RegM030: TRegistroM030): String;
var
intFor: integer;
strRegistroM355: String;
begin
  strRegistroM355 := '';

  if Assigned(RegM030.RegistroM355) then
  begin
     for intFor := 0 to RegM030.RegistroM355.Count - 1 do
     begin
        with RegM030.RegistroM355.Items[intFor] do
        begin
           ///
           strRegistroM355 :=  strRegistroM355 + LFill('M355') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_SLD_FIN_SOC, 19, 2) +
                                                 LFill(IND_DC_FIN_SOC) +
                                                 LFill(VL_DEB_FCONT_E, 19, 2) +
                                                 LFill(VL_CRED_FCONT_E, 19, 2) +
                                                 LFill(VL_DEB_FCONT_I, 19, 2) +
                                                 LFill(VL_CRED_FCONT_I, 19, 2) +
                                                 LFill(VL_SLD_FIN_FC_AL, 19, 2) +
                                                 LFill(IND_DC_FIN_FC_AL) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroM990.QTD_LIN_M := FRegistroM990.QTD_LIN_M + 1;
     end;
     FRegistroM355Count := FRegistroM355Count + RegM030.RegistroM355.Count;
  end;
  Result := strRegistroM355;
end;


function TBloco_M.WriteRegistroM990: String;
begin
  Result := '';

  if Assigned(FRegistroM990) then
  begin
     with FRegistroM990 do
     begin
       QTD_LIN_M := QTD_LIN_M + 1;
       ///
       Result := LFill('M990') +
                 LFill(QTD_LIN_M, 0) +
                 Delimitador +
                 #13#10;
     end;
  end;
end;

end.
