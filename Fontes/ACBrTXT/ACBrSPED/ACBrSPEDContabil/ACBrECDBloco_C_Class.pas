{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elton Barbosa                                   }
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

unit ACBrECDBloco_C_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrECDBloco_C, ACBrECDBloco_0_Class;

type
  /// TBloco_C -
  TBloco_C = class(TACBrSPED)
  private
    FRegistroC001: TRegistroC001;
    FRegistroC040: TRegistroC040;
    FRegistroC990: TRegistroC990;

    FRegistroC050Count: Integer;
    FRegistroC051Count: Integer;
    FRegistroC052Count: Integer;
    FRegistroC150Count: Integer;
    FRegistroC155Count: Integer;
    FRegistroC600Count: Integer;
    FRegistroC650Count: Integer;

    FBloco_0: TBloco_0;
    FGerarBlocoC: Boolean;

    procedure WriteRegistroC050(RegC040: TRegistroC040);
    procedure WriteRegistroC051(RegC050: TRegistroC050);
    procedure WriteRegistroC052(RegC050: TRegistroC050);
    procedure WriteRegistroC150(RegC040: TRegistroC040);
    procedure WriteRegistroC155(RegC150: TRegistroC150);
    procedure WriteRegistroC600(RegC040: TRegistroC040);
    procedure WriteRegistroC650(RegC600: TRegistroC600);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros; override;

    procedure WriteRegistroC001;
    procedure WriteRegistroC040;
    procedure WriteRegistroC990;

    property GerarBlocoC: Boolean read FGerarBlocoC write FGerarBlocoC;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroC001: TRegistroC001     read fRegistroC001 write fRegistroC001;
    property RegistroC040: TRegistroC040     read fRegistroC040 write fRegistroC040;
    property RegistroC990: TRegistroC990     read fRegistroC990 write fRegistroC990;

    property RegistroC050Count: Integer read FRegistroC050Count write FRegistroC050Count;
    property RegistroC051Count: Integer read FRegistroC051Count write FRegistroC051Count;
    property RegistroC052Count: Integer read FRegistroC052Count write FRegistroC052Count;
    property RegistroC150Count: Integer read FRegistroC150Count write FRegistroC150Count;
    property RegistroC155Count: Integer read FRegistroC155Count write FRegistroC155Count;
    property RegistroC600Count: Integer read FRegistroC600Count write FRegistroC600Count;
    property RegistroC650Count: Integer read FRegistroC650Count write FRegistroC650Count;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_C }

constructor TBloco_C.Create;
begin
  inherited Create;
  FGerarBlocoC := False;
  fRegistroC001 := TRegistroC001.Create;
  fRegistroC040 := TRegistroC040.Create;
  fRegistroC990 := TRegistroC990.Create;
  FRegistroC050Count := 0;
  FRegistroC051Count := 0;
  FRegistroC052Count := 0;
  FRegistroC150Count := 0;
  FRegistroC155Count := 0;
  FRegistroC600Count := 0;
  FRegistroC650Count := 0;

  fRegistroC990.QTD_LIN_C := 0;
end;

destructor TBloco_C.Destroy;
begin
  fRegistroC001.Free;
  fRegistroC040.Free;
  fRegistroC990.Free;
  inherited;
end;

procedure TBloco_C.LimpaRegistros;
begin
  FRegistroC050Count := 0;
  FRegistroC051Count := 0;
  FRegistroC052Count := 0;
  FRegistroC150Count := 0;
  FRegistroC155Count := 0;
  FRegistroC600Count := 0;
  FRegistroC650Count := 0;

  fRegistroC990.QTD_LIN_C := 0;
end;

procedure  TBloco_C.WriteRegistroC001;
begin
  if Assigned(fRegistroC001) then
  begin
     with fRegistroC001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(C-C001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add( LFill('C001') +
            LFill(IND_DAD, 1)
            );
       ///
       fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
     end;
  end;
end;

procedure TBloco_C.WriteRegistroC040;
begin
  if Assigned(fRegistroC040) then
  begin
    Self.Bloco_0.
    Add(LFill('C040') +
        LFill(fRegistroC040.HASH_ECD_REC, 40) +
        LFill(fRegistroC040.DT_INI_ECD_REC) +
        LFill(fRegistroC040.DT_FIN_ECD_REC) +
        LFill(fRegistroC040.CNPJ_ECD_REC, 14) +
        LFill(fRegistroC040.IND_ESC, 1) +
        LFill(fRegistroC040.COD_VER_LC) +
        LFill(fRegistroC040.NUM_ORD) +
        LFill(fRegistroC040.NAT_LIVR, 80) +
        LFill(fRegistroC040.IND_SIT_ESP_ECD_REC,1, True) +
        LFill(fRegistroC040.IND_NIRE_ECD_REC, 1) +
        LFill(fRegistroC040.IND_FIN_ESC_ECD_REC, 1) +
        LFill(fRegistroC040.TIP_ECD_REC, 1) +
        LFill(fRegistroC040.COD_SCP_ECD_REC, 14) +
        LFill(fRegistroC040.IDENT_MF_ECD_REC, 1) +
        LFill(fRegistroC040.IND_ESC_CONS_ECD_REC, 1) +
        LFill(fRegistroC040.IND_CENTRALIZADA_ECD_REC, 1) +
        LFill(fRegistroC040.IND_MUDANCA_PC_ECD_REC, 1) +
        LFill(fRegistroC040.IND_PLANO_REF_ECD_REC, 1)
       );

    FRegistroC990.QTD_LIN_C := FRegistroC990.QTD_LIN_C + 1;

    // Registro Filho
    WriteRegistroC050(FRegistroC040);
    WriteRegistroC150(FRegistroC040);
  end;
end;

procedure TBloco_C.WriteRegistroC050(RegC040: TRegistroC040);
var
  intFor: integer;
  UmRegistroC050: TRegistroC050;
begin
  if Assigned(RegC040.RegistroC050) then
  begin
    for intFor := 0 to RegC040.RegistroC050.Count - 1 do
    begin
      UmRegistroC050 := RegC040.RegistroC050.Items[intFor];
      Add( LFill('C050') +
           LFill(UmRegistroC050.DT_ALT) +
           LFill(UmRegistroC050.COD_NAT, 2) +
           LFill(UmRegistroC050.IND_CTA, 1) +
           LFill(UmRegistroC050.NIVEL) +
           LFill(UmRegistroC050.COD_CTA) +
           LFill(UmRegistroC050.COD_CTA_SUP) +
           LFill(UmRegistroC050.CTA) +
           LFill(UmRegistroC050.CTA) +
           LFill(UmRegistroC050.CTA) +
           LFill(UmRegistroC050.CTA)
         );

      WriteRegistroC051(UmRegistroC050);
      WriteRegistroC052(UmRegistroC050);

      fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
    end;
    FRegistroC050Count := FRegistroC050Count + RegC040.RegistroC050.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC051(RegC050: TRegistroC050);
var
  intFor: integer;
  UmRegistroC051: TRegistroC051;
begin
  if Assigned(RegC050.RegistroC051) then
  begin
    for intFor := 0 to RegC050.RegistroC051.Count - 1 do
    begin
      UmRegistroC051 := RegC050.RegistroC051.Items[intFor];

      Add( LFill('C051') +
           LFill(UmRegistroC051.COD_CCUS) +
           LFill(UmRegistroC051.COD_CTA_REF)
          );

      fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
    end;
    FRegistroC051Count := FRegistroC051Count + RegC050.RegistroC051.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC052(RegC050: TRegistroC050);
var
  intFor: integer;
  UmRegistroC052: TRegistroC052;
begin
  if Assigned(RegC050.RegistroC052) then
  begin
    for intFor := 0 to RegC050.RegistroC052.Count - 1 do
    begin
      UmRegistroC052 := RegC050.RegistroC052.Items[intFor];
      Add( LFill('C052') +
           LFill(UmRegistroC052.COD_CCUS) +
           LFill(UmRegistroC052.COD_AGL)
          );
      fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
     end;
     FRegistroC052Count := FRegistroC052Count + RegC050.RegistroC052.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC150(RegC040: TRegistroC040);
var
  intFor: integer;
  UmRegistroC150: TRegistroC150;
begin
  if Assigned(RegC040.RegistroC150) then
  begin
    for intFor := 0 to RegC040.RegistroC150.Count - 1 do
    begin
      UmRegistroC150 := RegC040.RegistroC150.Items[intFor];

      Add( LFill('C150') +
           LFill(UmRegistroC150.DT_INI) +
           LFill(UmRegistroC150.DT_FIN)
         );

      // Registro Filho
      WriteRegistroC155(UmRegistroC150);

      fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
    end;
    FRegistroC150Count := FRegistroC150Count + RegC040.RegistroC150.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC155(RegC150: TRegistroC150);
var
  intFor: integer;
  UmRegistroC155: TRegistroC155;
begin
  if Assigned(RegC150.RegistroC155) then
  begin
    for intFor := 0 to RegC150.RegistroC155.Count - 1 do
    begin
      UmRegistroC155 := RegC150.RegistroC155.Items[intFor];
      Add( LFill('C155') +
           LFill(UmRegistroC155.COD_CTA_REC) +
           LFill(UmRegistroC155.COD_CCUS_REC) +
           LFill(UmRegistroC155.VL_SLD_INI_REC, 19, 2) +
           LFill(UmRegistroC155.IND_DC_INI_REC, 1) +
           LFill(UmRegistroC155.VL_DEB_REC, 19, 2) +
           LFill(UmRegistroC155.VL_CRED_REC, 19, 2) +
           LFill(UmRegistroC155.VL_SLD_FIN_REC, 19, 2) +
           LFill(UmRegistroC155.IND_DC_FIN_REC, 1)
          );

      fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
    end;
    FRegistroC155Count := FRegistroC155Count + RegC150.RegistroC155.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC600(RegC040: TRegistroC040);
var
  intFor: integer;
  UmRegistroC600: TRegistroC600;
begin
  if Assigned(RegC040.RegistroC600) then
  begin
    for intFor := 0 to RegC040.RegistroC600.Count - 1 do
    begin
      UmRegistroC600 := RegC040.RegistroC600.Items[intFor];
      Add( LFill('C600') +
           LFill(UmRegistroC600.DT_INI) +
           LFill(UmRegistroC600.DT_FIN) +
           LFill(UmRegistroC600.ID_DEM, 1) +
           LFill(UmRegistroC600.CAB_DEM)
          );
      // Registro Filho
      WriteRegistroC650(RegC040.RegistroC600.Items[intFor]);

      fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
    end;
    FRegistroC600Count := FRegistroC600Count + RegC040.RegistroC600.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC650(RegC600: TRegistroC600);
var
  intFor: integer;
  UmRegistroC650: TRegistroC650;
begin
  if Assigned(RegC600.RegistroC650) then
  begin
    for intFor := 0 to RegC600.RegistroC650.Count - 1 do
    begin
      UmRegistroC650 := RegC600.RegistroC650.Items[intFor];
      Add( LFill('C650') +
           LFill(UmRegistroC650.COD_AGL) +
           LFill(UmRegistroC650.NIVEL_AGL) +
           LFill(UmRegistroC650.DESCR_COD_AGL) +
           LFill(UmRegistroC650.VL_CTA_FIN, 19, 2) +
           LFill(UmRegistroC650.IND_DC_CTA_FIN)
         );

      fRegistroC990.QTD_LIN_C := fRegistroC990.QTD_LIN_C + 1;
    end;
    FRegistroC650Count := FRegistroC650Count + RegC600.RegistroC650.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC990;
var
  strLinha : String;
begin
  if Assigned(fRegistroC990) then
  begin
     with fRegistroC990 do
     begin
       QTD_LIN_C := QTD_LIN_C + 1;
       ///
       strLinha := LFill('C990') +
            LFill(QTD_LIN_C, 0);
       Add(strLinha);
     end;
  end;
end;

end.
