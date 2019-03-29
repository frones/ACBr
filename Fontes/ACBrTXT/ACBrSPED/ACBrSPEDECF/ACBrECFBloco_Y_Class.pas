{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					                    2015   Isaque Pinheiro	    	             }
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
|* 27/08/2015 - Ariel Guareschi - Alterado a geração do arquivo bloco Y
|* 11/09/2015 - Ariel Guareschi - Identar no padrao utilizado pela ACBr
|* 22/06/2017 - Ariel Guareschi - Alterado p/gerar bloco Y800 quando informado 
*******************************************************************************}


{$I ACBr.inc}

unit ACBrECFBloco_Y_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_Y, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_Y -

  { TBloco_Y }

  TBloco_Y = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroY001: TRegistroY001;
    FRegistroY990: TRegistroY990;

    FRegistroY681Count: Integer;
    FRegistroY650Count: Integer;

    procedure WriteRegistroY650(RegY640: TRegistroY640);
    procedure WriteRegistroY681(RegY680: TRegistroY680);

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    function RegistroY001New: TRegistroY001;
    function RegistroY520New: TRegistroY520;
    function RegistroY540New: TRegistroY540;
    function RegistroY550New: TRegistroY550;
    function RegistroY560New: TRegistroY560;
    function RegistroY570New: TRegistroY570;
    function RegistroY580New: TRegistroY580;
    function RegistroY590New: TRegistroY590;
    function RegistroY600New: TRegistroY600;
    function RegistroY611New: TRegistroY611;
    function RegistroY612New: TRegistroY612;
    function RegistroY620New: TRegistroY620;
    function RegistroY630New: TRegistroY630;
    function RegistroY640New: TRegistroY640;
    function RegistroY650New: TRegistroY650;
    function RegistroY660New: TRegistroY660;
    function RegistroY665New: TRegistroY665;
    function RegistroY671New: TRegistroY671;
    function RegistroY672New: TRegistroY672;
    function RegistroY680New: TRegistroY680;
    function RegistroY681New: TRegistroY681;
    function RegistroY682New: TRegistroY682;
    function RegistroY690New: TRegistroY690;
    function RegistroY800New: TRegistroY800;

    procedure WriteRegistroY001;
    procedure WriteRegistroY520;
    procedure WriteRegistroY540;
    procedure WriteRegistroY550;
    procedure WriteRegistroY560;
    procedure WriteRegistroY570;
    procedure WriteRegistroY580;
    procedure WriteRegistroY590;
    procedure WriteRegistroY600;
    procedure WriteRegistroY611;
    procedure WriteRegistroY612;
    procedure WriteRegistroY620;
    procedure WriteRegistroY630;
    procedure WriteRegistroY640;
    procedure WriteRegistroY660;
    procedure WriteRegistroY665;
    procedure WriteRegistroY671;
    procedure WriteRegistroY672;
    procedure WriteRegistroY680;
    procedure WriteRegistroY682;
    procedure WriteRegistroY690;
    procedure WriteRegistroY800;
    procedure WriteRegistroY990;

    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros; override;

    property RegistroY001: TRegistroY001     read FRegistroY001 write FRegistroY001;
    property RegistroY990: TRegistroY990     read FRegistroY990 write FRegistroY990;

    property RegistroY650Count: Integer read FRegistroY650Count write FRegistroY650Count;
    property RegistroY681Count: Integer read FRegistroY681Count write FRegistroY681Count;
  end;

implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_Y }

constructor TBloco_Y.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TBloco_Y.CriaRegistros;
begin
  inherited;
  FRegistroY001 := TRegistroY001.Create;
  FRegistroY990 := TRegistroY990.Create;

  FRegistroY650Count := 0;
  FRegistroY681Count := 0;

  FRegistroY990.QTD_LIN := 0;
end;

destructor TBloco_Y.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_Y.LiberaRegistros;
begin
  inherited;
  FRegistroY001.Free;
  FRegistroY990.Free;
end;

procedure TBloco_Y.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;


function TBloco_Y.RegistroY001New: TRegistroY001;
begin
  Result := FRegistroY001;
end;

function TBloco_Y.RegistroY520New: TRegistroY520;
begin
  Result := FRegistroY001.RegistroY520.New;
end;

function TBloco_Y.RegistroY540New: TRegistroY540;
begin
  Result := FRegistroY001.RegistroY540.New;
end;

function TBloco_Y.RegistroY550New: TRegistroY550;
begin
  Result := FRegistroY001.RegistroY550.New;
end;

function TBloco_Y.RegistroY560New: TRegistroY560;
begin
  Result := FRegistroY001.RegistroY560.New;
end;

function TBloco_Y.RegistroY570New: TRegistroY570;
begin
  Result := FRegistroY001.RegistroY570.New;
end;

function TBloco_Y.RegistroY580New: TRegistroY580;
begin
  Result := FRegistroY001.RegistroY580.New;
end;

function TBloco_Y.RegistroY590New: TRegistroY590;
begin
  Result := FRegistroY001.RegistroY590.New;
end;

function TBloco_Y.RegistroY600New: TRegistroY600;
begin
  Result := FRegistroY001.RegistroY600.New;
end;

function TBloco_Y.RegistroY611New: TRegistroY611;
begin
  Result := FRegistroY001.RegistroY611.New;
end;

function TBloco_Y.RegistroY612New: TRegistroY612;
begin
  Result := FRegistroY001.RegistroY612.New;
end;

function TBloco_Y.RegistroY620New: TRegistroY620;
begin
  Result := FRegistroY001.RegistroY620.New;
end;

function TBloco_Y.RegistroY630New: TRegistroY630;
begin
  Result := FRegistroY001.RegistroY630.New;
end;

function TBloco_Y.RegistroY640New: TRegistroY640;
begin
  Result := FRegistroY001.RegistroY640.New;
end;

function TBloco_Y.RegistroY650New: TRegistroY650;
var
  UN640: TRegistroY640;
  UN640Count: Integer;
begin
  UN640Count := FRegistroY001.RegistroY640.Count -1;
  if UN640Count = -1 then
    raise Exception.Create('O registro Y650 deve ser filho do registro Y640, e não existe nenhum Y640 pai!');

  UN640  := FRegistroY001.RegistroY640.Items[UN640Count];
  Result := UN640.RegistroY650.New;
end;

function TBloco_Y.RegistroY660New: TRegistroY660;
begin
  Result := FRegistroY001.RegistroY660.New;
end;

function TBloco_Y.RegistroY665New: TRegistroY665;
begin
  Result := FRegistroY001.RegistroY665.New;
end;

function TBloco_Y.RegistroY671New: TRegistroY671;
begin
  Result := FRegistroY001.RegistroY671.New;
end;

function TBloco_Y.RegistroY672New: TRegistroY672;
begin
  Result := FRegistroY001.RegistroY672.New;
end;

function TBloco_Y.RegistroY680New: TRegistroY680;
begin
  Result := FRegistroY001.RegistroY680.New;
end;

function TBloco_Y.RegistroY681New: TRegistroY681;
var
  UN680: TRegistroY680;
  UN680Count: Integer;
begin
  UN680Count := FRegistroY001.RegistroY680.Count -1;
  if UN680Count = -1 then
    raise Exception.Create('O registro Y681 deve ser filho do registro Y680, e não existe nenhum Y680 pai!');

  UN680  := FRegistroY001.RegistroY680.Items[UN680Count];
  Result := UN680.RegistroY681.New;
end;

function TBloco_Y.RegistroY682New: TRegistroY682;
begin
  Result := FRegistroY001.RegistroY682.New;
end;

function TBloco_Y.RegistroY690New: TRegistroY690;
begin
  Result := FRegistroY001.RegistroY690.New;
end;

function TBloco_Y.RegistroY800New: TRegistroY800;
begin
  Result := FRegistroY001.RegistroY800;
end;

procedure TBloco_Y.WriteRegistroY001;
begin
  if Assigned(FRegistroY001) then
  begin
    with FRegistroY001 do
    begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(Y-Y001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
      Add(LFill('Y001') +
          LFill( Integer(IND_DAD), 1));
      FRegistroY990.QTD_LIN:= FRegistroY990.QTD_LIN + 1;
    end;

    WriteRegistroY520;
    WriteRegistroY540;
    WriteRegistroY550;
    WriteRegistroY560;
    WriteRegistroY570;
    WriteRegistroY580;
    WriteRegistroY590;
    WriteRegistroY600;
    WriteRegistroY611;
    WriteRegistroY612;
    WriteRegistroY620;
    WriteRegistroY630;
    WriteRegistroY640;
    WriteRegistroY660;
    WriteRegistroY665;
    WriteRegistroY671;
    WriteRegistroY672;
    WriteRegistroY680;
    WriteRegistroY682;
    WriteRegistroY690;
    WriteRegistroY800;
  end;
end;

procedure TBloco_Y.WriteRegistroY520;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY520) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY520.Count - 1 do
    begin
      with FRegistroY001.RegistroY520.Items[intFor] do
      begin
        Add(LFill('Y520') +
            LFill(TIP_EXT) +
            LFill(PAIS,3) +
            LFill(FORMA,1) +
            LFill(NAT_OPER,5) +
            VLFill(VL_PERIODO, 19, 2));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY540;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY540) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY540.Count - 1 do
    begin
      with FRegistroY001.RegistroY540.Items[intFor] do
      begin
        Add(LFill('Y540') +
            LFill(CNPJ_ESTAB, 14) +
            VLFill(VL_REC_ESTAB, 19, 2) +
            LFill(CNAE, 7));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY550;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY550) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY550.Count - 1 do
    begin
      with FRegistroY001.RegistroY550.Items[intFor] do
      begin
        Add(LFill('Y550') +
            LFill(CNPJ_EXP, 14) +
            LFill(COD_NCM,8) +
            VLFill(VL_VENDA, 19, 2));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY560;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY560) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY560.Count - 1 do
    begin
      with FRegistroY001.RegistroY560.Items[intFor] do
      begin
        Add(LFill('Y560') +
            LFill(CNPJ, 14) +
            LFill(COD_NCM,8) +
            VLFill(VL_COMPRA, 19, 2) +
            VLFill(VL_EXP, 19, 2));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY570;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY570) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY570.Count - 1 do
    begin
      with FRegistroY001.RegistroY570.Items[intFor] do
      begin
        Add(LFill('Y570') +
            LFill(CNPJ_FON, 14) +
            LFill(NOM_EMP) +
            LFill(IND_ORG_PUB) +
            LFill(COD_REC, 4) +
            VLFill(VL_REND, 19, 2) +
            VLFill(IR_RET, 19, 2) +
            VLFill(CSLL_RET, 19, 2));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY580;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY580) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY580.Count - 1 do
    begin
      with FRegistroY001.RegistroY580.Items[intFor] do
      begin
        Add(LFill('Y580') +
            LFill(CNPJ, 14) +
            LFill(TIP_BENEF, 1) +
            LFill(FORM_DOA, 1) +
            VLFill(VL_DOA, 19, 2))
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY590;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY590) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY590.Count - 1 do
    begin
      with FRegistroY001.RegistroY590.Items[intFor] do
      begin
        Add(LFill('Y590') +
            LFill(TIP_ATIVO) +
            LFill(PAIS) +
            LFill(DISCRIMINACAO) +
            VLFill(VL_ANT, 19, 2) +
            VLFill(VL_ATUAL, 19, 2));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY600;
var
  intFor: integer;
  strQUALIF_REP_LEG : String;
begin
  if Assigned(FRegistroY001.RegistroY600) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY600.Count - 1 do
    begin
      with FRegistroY001.RegistroY600.Items[intFor] do
      begin
        case QUALIF_REP_LEG of
          qrlNenhum     : strQUALIF_REP_LEG := '';
          qrlProcurador : strQUALIF_REP_LEG := '01';
          qrlCurador    : strQUALIF_REP_LEG := '02';
          qrlMae        : strQUALIF_REP_LEG := '03';
          qrlPai        : strQUALIF_REP_LEG := '04';
          qrlTutor      : strQUALIF_REP_LEG := '05';
          qrlOutro      : strQUALIF_REP_LEG := '06';
        end;

        if Bloco_0.Registro0000.COD_VER >= ECFVersao200 then //Lay-Out 002 (devsyspro)
        begin
          Add(LFill('Y600') +
              LFill(DT_ALT_SOC) +
              LFill(DT_FIM_SOC) +
              LFill(PAIS,3) +
              LFill(IND_QUALIF_SOCIO) +
              LFill(CPF_CNPJ) +
              LFill(NOM_EMP) +
              LFill(QUALIF) +
              VLFill(PERC_CAP_TOT, 4, 2) +
              VLFill(PERC_CAP_VOT, 4, 2) +
              LFill(CPF_REP_LEG) +
              LFill(strQUALIF_REP_LEG) +
              VLFill(VL_REM_TRAB, 19, 2) +
              VLFill(VL_LUC_DIV, 19, 2) +
              VLFill(VL_JUR_CAP, 19, 2) +
              VLFill(VL_DEM_REND, 19, 2) +
              VLFill(VL_IR_RET, 19, 2)
              );
        end
        else // Lay-Out 001 (devsyspro)
        begin
          Add(LFill('Y600') +
              LFill(DT_ALT_SOC) +
              LFill(DT_FIM_SOC) +
              LFill(PAIS,3) +
              LFill(IND_QUALIF_SOCIO) +
              LFill(CPF_CNPJ) +
              LFill(NOM_EMP) +
              LFill(QUALIF) +
              VLFill(PERC_CAP_TOT, 4, 2) +
              VLFill(PERC_CAP_VOT, 4, 2) +
              LFill(CPF_REP_LEG) +
              LFill(strQUALIF_REP_LEG)
              );
        end;
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY611;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY611) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY611.Count - 1 do
    begin
      with FRegistroY001.RegistroY611.Items[intFor] do
      begin
        Add(LFill('Y611') +
            LFill(PAIS,3) +
            LFill(IND_PF_PJ) +
            LFill(CPF_CNPJ) +
            LFill(NOM_EMP) +
            LFill(QUALIF) +
            VLFill(VL_REM_TRAB, 19, 2) +
            VLFill(VL_LUC_DIV, 19, 2) +
            VLFill(VL_JUR_CAP, 19, 2) +
            VLFill(VL_DEM_REND, 19, 2) +
            VLFill(VL_IR_RET, 19, 2));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY612;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY612) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY612.Count - 1 do
    begin
      with FRegistroY001.RegistroY612.Items[intFor] do
      begin
        Add(LFill('Y612') +
            LFill(CPF) +
            LFill(NOME) +
            LFIll(QUALIF,2) +
            VLFill(VL_REM_TRAB, 19, 2) +
            VLFill(VL_DEM_REND, 19, 2) +
            VLFill(VL_IR_RET, 19, 2));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY620;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY620) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY620.Count - 1 do
    begin
      with FRegistroY001.RegistroY620.Items[intFor] do
      begin
        Add(LFill('Y620') +
            LFill(DT_EVENTO) +
            LFill(IND_RELAC, 1) +
            LFill(PAIS, 3) +
            LFill(CNPJ, 14, True) +
            LFill(NOM_EMP) +
            VLFill(VALOR_REAIS, 19, 2) +
            VLFill(VALOR_ESTR, 19, 2) +
            VLFill(PERC_CAP_TOT, 4, 2) +
            VLFill(PERC_CAP_VOT, 4, 2) +
            VLFill(RES_EQ_PAT, 19, 2) +
            LFill(DATA_AQUIS) +
            LFill(IND_PROC_CART) +
            LFill(NUM_PROC_CART) +
            LFill(NOME_CART) +
            LFill(IND_PROC_RFB) +
            LFill(NUM_PROC_RFB));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY630;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY630) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY630.Count - 1 do
    begin
      with FRegistroY001.RegistroY630.Items[intFor] do
      begin
        Add(LFill('Y630') +
            LFill(CNPJ, 14) +
            LFill(QTE_QUOT) +
            LFill(QTE_QUOTA) +
            VLFill(PATR_FIN_PER, 19, 2) +
            LFill(DAT_ABERT) +
            LFill(DAT_ENCER));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY640;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY640) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY640.Count - 1 do
    begin
      with FRegistroY001.RegistroY640.Items[intFor] do
      begin
        Add(LFill('Y640') +
            LFill(CNPJ, 14) +
            LFill(COND_DECL, 1) +
            VLFill(VL_CONS, 19, 2) +
            LFill(CNPJ_LID, 14) +
            VLFill(VL_DECL,19));
      end;

      // Registros Filhos
      WriteRegistroY650(FRegistroY001.RegistroY640.Items[intFor]);

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY650(RegY640: TRegistroY640);
var
  intFor: integer;
begin
  if Assigned(RegY640.RegistroY650) then
  begin
    for intFor := 0 to RegY640.RegistroY650.Count - 1 do
    begin
      with RegY640.RegistroY650.Items[intFor] do
      begin
        Add(LFill('Y650') +
            LFill(CNPJ, 14) +
            VLFill(VL_PART, 19, 2));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;

    FRegistroY650Count := FRegistroY650Count + RegY640.RegistroY650.Count;
  end;
end;

procedure TBloco_Y.WriteRegistroY660;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY660) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY660.Count - 1 do
    begin
      with FRegistroY001.RegistroY660.Items[intFor] do
      begin
        Add(LFill('Y660') +
            LFill(CNPJ, 14) +
            LFill(NOM_EMP) +
            VLFill(PERC_PAT_LIQ, 5, 2));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY665;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY665) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY665.Count - 1 do
    begin
      with FRegistroY001.RegistroY665.Items[intFor] do
      begin
        Add(LFill('Y665') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(DESC_CTA) +
            VLFill(VL_SALDO_SOC, 19, 2) +
            LFill(IND_VL_SALDO_SOC) +
            VLFill(VL_SALDO_FIS, 19, 2) +
            LFill(IND_VL_SALDO_FIS) +
            VLFill(DIF_SALDOS, 19, 2) +
            LFill(IND_DIF_SALDOS) +
            LFill(MET_CONTR) +
            LFill(COD_SUBCONT) +
            LFill(COD_CCUS_SUB));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY671;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY671) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY671.Count - 1 do
    begin
      with FRegistroY001.RegistroY671.Items[intFor] do
      begin
        Add(LFill('Y671') +
            VLFill(VL_AQ_MAQ, 19, 2) +
            VLFill(VL_DOA_CRIANCA, 19, 2) +
            VLFill(VL_DOA_IDOSO, 19, 2) +
            VLFill(VL_AQ_IMOBILIZADO, 19, 2) +
            VLFill(VL_BX_IMOBILIZADO, 19, 2) +
            VLFill(VL_INC_INI, 19, 2) +
            VLFill(VL_INC_FIN, 19, 2) +
            VLFill(VL_CSLL_DEPREC_INI, 19, 2) +
            VLFill(VL_OC_SEM_IOF, 19, 2) +
            VLFill(VL_FOLHA_ALIQ_RED, 19, 2) +
            VLFill(VL_ALIQ_RED, 4, 2) +
            LFill(IND_ALTER_CAPITAL, 1) +
            LFill(IND_BCN_CSLL, 1));
      end;
      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY672;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY672) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY672.Count - 1 do
    begin
      with FRegistroY001.RegistroY672.Items[intFor] do
      begin
        if Bloco_0.Registro0000.COD_VER >= ECFVersao500 then
          Add(LFill('Y672') +
              VLFill(VL_CAPITAL_ANT, 19, 2) +
              VLFill(VL_CAPITAL, 19, 2) +
              VLFill(VL_ESTOQUE_ANT, 19, 2) +
              VLFill(VL_ESTOQUES, 19, 2) +
              VLFill(VL_CAIXA_ANT, 19, 2) +
              VLFill(VL_CAIXA, 19, 2) +
              VLFill(VL_APLIC_FIN_ANT, 19, 2) +
              VLFill(VL_APLIC_FIN, 19, 2) +
              VLFill(VL_CTA_REC_ANT, 19, 2) +
              VLFill(VL_CTA_REC, 19, 2) +
              VLFill(VL_CTA_PAG_ANT, 19, 2) +
              VLFill(VL_CTA_PAG, 19, 2) +
              VLFill(VL_COMPRA_MERC, 19, 2) +
              VLFill(VL_COMPRA_ATIVO, 19, 2) +
              VLFill(VL_RECEITAS, 19, 2) +
              VLFill(TOT_ATIVO, 19, 2) +
              LFill(IND_AVAL_ESTOQ, 1))
        else
            Add(LFill('Y672') +
              VLFill(VL_CAPITAL_ANT, 19, 2) +
              VLFill(VL_CAPITAL, 19, 2) +
              VLFill(VL_ESTOQUE_ANT, 19, 2) +
              VLFill(VL_ESTOQUES, 19, 2) +
              VLFill(VL_CAIXA_ANT, 19, 2) +
              VLFill(VL_CAIXA, 19, 2) +
              VLFill(VL_APLIC_FIN_ANT, 19, 2) +
              VLFill(VL_APLIC_FIN, 19, 2) +
              VLFill(VL_CTA_REC_ANT, 19, 2) +
              VLFill(VL_CTA_REC, 19, 2) +
              VLFill(VL_CTA_PAG_ANT, 19, 2) +
              VLFill(VL_CTA_PAG, 19, 2) +
              VLFill(VL_COMPRA_MERC, 19, 2) +
              VLFill(VL_COMPRA_ATIVO, 19, 2) +
              VLFill(VL_RECEITAS, 19, 2) +
              VLFill(TOT_ATIVO, 19, 2) +
              VLFill(VL_FOLHA, 19, 2) +
              VLFill(VL_ALIQ_RED, 4, 2) +
              ifThen( FBloco_0.Registro0000.COD_VER in [ ECFVersao100, ECFVersao200 ], LFill(IND_REG_APUR, 1), '' ) +
              LFill(IND_AVAL_ESTOQ, 1));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY680;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY680) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY680.Count - 1 do
    begin
      with FRegistroY001.RegistroY680.Items[intFor] do
      begin
        Add(LFill('Y680') +
            LFill(MES, 2));
      end;

      // Registros Filhos
      WriteRegistroY681(FRegistroY001.RegistroY680.Items[intFor]);

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY681(RegY680: TRegistroY680);
var
  intFor: integer;
begin
  if Assigned(RegY680.RegistroY681) then
  begin
    for intFor := 0 to RegY680.RegistroY681.Count - 1 do
    begin
      with RegY680.RegistroY681.Items[intFor] do
      begin
        Add(LFill('Y681') +
            LFill(CODIGO) +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
    FRegistroY681Count := FRegistroY681Count + RegY680.RegistroY681.Count;
  end;
end;

procedure TBloco_Y.WriteRegistroY682;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY682) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY682.Count - 1 do
    begin
      with FRegistroY001.RegistroY682.Items[intFor] do
      begin
        Add(LFill('Y682') +
            LFill(MES, 2) +
            VLFill(ACRES_PATR, 19, 2));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY690;
var
  intFor: integer;
begin
  if Assigned(FRegistroY001.RegistroY690) then
  begin
    for intFor := 0 to FRegistroY001.RegistroY690.Count - 1 do
    begin
      with FRegistroY001.RegistroY690.Items[intFor] do
      begin
        Add(LFill('Y690') +
            LFill(MES, 2) +
            VLFill(VL_REC_BRU, 19, 2));
      end;

      FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY800;
begin
  if Assigned(FRegistroY001.RegistroY800) then
  begin
    with FRegistroY001.RegistroY800 do
    begin
      if (FRegistroY001.RegistroY800.ARQ_RTF <> '') then
      begin
        case Bloco_0.Registro0000.COD_VER of

          ECFVersao100, ECFVersao200:
              Add(LFill('Y800') +
                  LFill(ARQ_RTF) +
                  LFill(IND_FIM_RTF));

          ECFVersao300, ECFVersao400, ECFVersao500:
               Add( LFill('Y800') +
                    LFill(TIPO_DOC) +
                    LFill(DESC_RTF) +
                    LFill(HASH_RTF) +
                    LFill(ARQ_RTF) +
                    LFill(IND_FIM_RTF)
                    );
        end;
        FRegistroY990.QTD_LIN := FRegistroY990.QTD_LIN + 1;
      end;
    end;
  end;
end;

procedure TBloco_Y.WriteRegistroY990;
begin
  if Assigned(FRegistroY990) then
  begin
    with FRegistroY990 do
    begin
      QTD_LIN := QTD_LIN + 1;
      Add(LFill('Y990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
