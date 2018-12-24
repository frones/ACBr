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

unit ACBrFContBloco_I_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrFContBloco_I;

type
  /// TBLOCO_I -
  TBLOCO_I = class(TACBrSPED)
  private
    FRegistroI001: TRegistroI001;      /// BLOCO I - RegistroI001
    FRegistroI050: TRegistroI050List;  /// BLOCO I - Lista de RegistroI050
    FRegistroI075: TRegistroI075List;  /// BLOCO I - Lista de RegistroI075
    FRegistroI100: TRegistroI100List;  /// BLOCO I - Lista de RegistroI100
    FRegistroI150: TRegistroI150List;  /// BLOCO I - Lista de RegistroI150
    FRegistroI155: TRegistroI155List;  /// BLOCO I - Lista de RegistroI150
    FRegistroI200: TRegistroI200List;
    FRegistroI250: TRegistroI250List;
    FRegistroI350: TRegistroI350List;  /// BLOCO I - Lista de RegistroI350
    FRegistroI355: TRegistroI355List;  /// BLOCO I - Lista de RegistroI350
    FRegistroI990: TRegistroI990;      /// BLOCO I - FRegistroI990

    FRegistroI051Count: Integer;
    FRegistroI155Count: Integer;
    FRegistroI250Count: Integer;
    FRegistroI355Count: Integer;
    FRegistroI156Count: Integer;
    FRegistroI256Count: Integer;
    FRegistroI356Count: Integer;

    function WriteRegistroI051(RegI050: TRegistroI050): String;
    function WriteRegistroI155(RegI150: TRegistroI150): String;
    function WriteRegistroI250(RegI200: TRegistroI200): String;
    function WriteRegistroI355(RegI350: TRegistroI350): String;
    function WriteRegistroI156(RegI155: TRegistroI155): String;
    function WriteRegistroI256(RegI250: TRegistroI250): String;
    function WriteRegistroI356(RegI355: TRegistroI355): String;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function WriteRegistroI001: String;
    function WriteRegistroI050: String;
    function WriteRegistroI075: String;
    function WriteRegistroI100: String;
    function WriteRegistroI150: String;
    function WriteRegistroI200: String;
    function WriteRegistroI350: String;
    function WriteRegistroI990: String;

    property RegistroI001: TRegistroI001     read FRegistroI001 write FRegistroI001;
    property RegistroI050: TRegistroI050List read fRegistroI050 write fRegistroI050;
    property RegistroI075: TRegistroI075List read fRegistroI075 write fRegistroI075;
    property RegistroI100: TRegistroI100List read fRegistroI100 write fRegistroI100;
    property RegistroI150: TRegistroI150List read fRegistroI150 write fRegistroI150;
    property RegistroI200: TRegistroI200List read fRegistroI200 write fRegistroI200;
    property RegistroI350: TRegistroI350List read fRegistroI350 write fRegistroI350;
    property RegistroI155: TRegistroI155List read fRegistroI155 write fRegistroI155;
    property RegistroI250: TRegistroI250List read fRegistroI250 write fRegistroI250;
    property RegistroI355: TRegistroI355List read fRegistroI355 write fRegistroI355;
    property RegistroI990: TRegistroI990     read FRegistroI990 write FRegistroI990;

    property RegistroI051Count: Integer read FRegistroI051Count write FRegistroI051Count;
    property RegistroI155Count: Integer read FRegistroI155Count write FRegistroI155Count;
    property RegistroI250Count: Integer read FRegistroI250Count write FRegistroI250Count;
    property RegistroI355Count: Integer read FRegistroI355Count write FRegistroI355Count;
    property RegistroI156Count: Integer read FRegistroI156Count write FRegistroI156Count;
    property RegistroI256Count: Integer read FRegistroI256Count write FRegistroI256Count;
    property RegistroI356Count: Integer read FRegistroI356Count write FRegistroI356Count;
  end;

implementation

{ TBLOCO_I }

constructor TBLOCO_I.Create;
begin
  inherited Create;
  FRegistroI001 := TRegistroI001.Create;
  FRegistroI050 := TRegistroI050List.Create;
  FRegistroI075 := TRegistroI075List.Create;
  FRegistroI100 := TRegistroI100List.Create;
  FRegistroI150 := TRegistroI150List.Create;
  FRegistroI200 := TRegistroI200List.Create;
  FRegistroI350 := TRegistroI350List.Create;
  FRegistroI155 := TRegistroI155List.Create;
  FRegistroI250 := TRegistroI250List.Create;
  FRegistroI355 := TRegistroI355List.Create;
  FRegistroI990 := TRegistroI990.Create;

  FRegistroI051Count := 0;
  FRegistroI155Count := 0;
  FRegistroI250Count := 0;
  FRegistroI355Count := 0;
  FRegistroI156Count := 0;
  FRegistroI256Count := 0;
  FRegistroI356Count := 0;

  FRegistroI990.QTD_LIN_I := 0;
end;

destructor TBLOCO_I.Destroy;
begin
  FRegistroI001.Free;
  FRegistroI050.Free;
  FRegistroI075.Free;
  FRegistroI100.Free;
  FRegistroI150.Free;
  FRegistroI200.Free;
  FRegistroI350.Free;
  FRegistroI155.Free;
  FRegistroI250.Free;
  FRegistroI355.Free;

  FRegistroI990.Free;
  inherited;
end;

procedure TBLOCO_I.LimpaRegistros;
begin
  FRegistroI050.Clear;
  FRegistroI075.Clear;
  FRegistroI100.Clear;
  FRegistroI150.Clear;
  FRegistroI200.Clear;
  FRegistroI350.Clear;
  FRegistroI155.Clear;
  FRegistroI250.Clear;
  FRegistroI355.Clear;

  FRegistroI990.QTD_LIN_I := 0;
end;

function TBLOCO_I.WriteRegistroI001: String;
begin
  Result := '';

  if Assigned(FRegistroI001) then
  begin
     with FRegistroI001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(I-I001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('I001') +
                 LFill(IND_DAD, 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
end;

function TBloco_I.WriteRegistroI050: String;
var
intFor: integer;
strRegistroI050: String;
begin
  strRegistroI050 := '';

  if Assigned(FRegistroI050) then
  begin
     for intFor := 0 to FRegistroI050.Count - 1 do
     begin
        with FRegistroI050.Items[intFor] do
        begin
           ///
           strRegistroI050 :=  strRegistroI050 + LFill('I050') +
                                                 LFill(DT_ALT) +
                                                 LFill(COD_NAT, 2) +
                                                 LFill(IND_CTA, 1) +
                                                 LFill(NIVEL) +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CTA_SUP) +
                                                 LFill(CTA) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registros Filhos
        strRegistroI050 := strRegistroI050 +
                           WriteRegistroI051(FRegistroI050.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI050;
end;

function TBloco_I.WriteRegistroI051(RegI050: TRegistroI050): String;
var
intFor: integer;
strRegistroI051: String;
begin
  strRegistroI051 := '';

  if Assigned(RegI050.RegistroI051) then
  begin
     for intFor := 0 to RegI050.RegistroI051.Count - 1 do
     begin
        with RegI050.RegistroI051.Items[intFor] do
        begin
           ///
           strRegistroI051 :=  strRegistroI051 + LFill('I051') +
                                                 LFill(COD_ENT_REF, 2) +
                                                 LFill(COD_CCUS) +
                                                 LFill(COD_CTA_REF) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI051Count := FRegistroI051Count + RegI050.RegistroI051.Count;
  end;
  Result := strRegistroI051;
end;

function TBloco_I.WriteRegistroI075: String;
var
intFor: integer;
strRegistroI075: String;
begin
  strRegistroI075 := '';

  if Assigned(RegistroI075) then
  begin
     for intFor := 0 to RegistroI075.Count - 1 do
     begin
        with RegistroI075.Items[intFor] do
        begin
           ///
           strRegistroI075 :=  strRegistroI075 + LFill('I075') +
                                                 LFill(COD_HIST) +
                                                 LFill(DESCR_HIST) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI075;
end;

function TBloco_I.WriteRegistroI100: String;
var
intFor: integer;
strRegistroI100: String;
begin
  strRegistroI100 := '';

  if Assigned(RegistroI100) then
  begin
     for intFor := 0 to RegistroI100.Count - 1 do
     begin
        with RegistroI100.Items[intFor] do
        begin
           ///
           strRegistroI100 :=  strRegistroI100 + LFill('I100') +
                                                 LFill(DT_ALT) +
                                                 LFill(COD_CCUS) +
                                                 LFill(CCUS) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI100;
end;

function TBloco_I.WriteRegistroI150: String;
var
intFor: integer;
strRegistroI150: String;
begin
  strRegistroI150 := '';

  if Assigned(RegistroI150) then
  begin
     for intFor := 0 to RegistroI150.Count - 1 do
     begin
        with RegistroI150.Items[intFor] do
        begin
           ///
           strRegistroI150 :=  strRegistroI150 + LFill('I150') +
                                                 LFill(DT_INI) +
                                                 LFill(DT_FIN) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI150 := strRegistroI150 +
                           WriteRegistroI155(RegistroI150.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI150;
end;

function TBloco_I.WriteRegistroI155(RegI150: TRegistroI150): String;
var
intFor: integer;
strRegistroI155: String;
begin
  strRegistroI155 := '';

  if Assigned(RegI150.RegistroI155) then
  begin
     for intFor := 0 to RegI150.RegistroI155.Count - 1 do
     begin
        with RegI150.RegistroI155.Items[intFor] do
        begin
           Check(((IND_DC_INI = 'D') or (IND_DC_INI = 'C') or (IND_DC_INI = '')), '(I-I155) No Indicador da situação do saldo inicial, deve ser informado: D ou C ou nulo!');
           Check(((IND_DC_FIN = 'D') or (IND_DC_FIN = 'C') or (IND_DC_FIN = '')), '(I-I155) No Indicador da situação do saldo inicial, deve ser informado: D ou C ou nulo!');
           ///
           strRegistroI155 :=  strRegistroI155 + LFill('I155') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(VL_SLD_INI, 19, 2) +
                                                 LFill(IND_DC_INI, 0) +
                                                 LFill(VL_DEB, 19, 2) +
                                                 LFill(VL_CRED, 19, 2) +
                                                 LFill(VL_SLD_FIN, 19, 2) +
                                                 LFill(IND_DC_FIN, 0) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI155 := strRegistroI155 +
                           WriteRegistroI156(RegI150.RegistroI155.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI155Count := FRegistroI155Count + RegI150.RegistroI155.Count;
  end;
  Result := strRegistroI155;
end;

function TBloco_I.WriteRegistroI156(RegI155: TRegistroI155): String;
var
intFor: integer;
strRegistroI156: String;
begin
  strRegistroI156 := '';

  if Assigned(RegI155.RegistroI156) then
  begin
     for intFor := 0 to RegI155.RegistroI156.Count - 1 do
     begin
        with RegI155.RegistroI156.Items[intFor] do
        begin
           strRegistroI156 :=  strRegistroI156 + LFill('I156') +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_DEB, 19, 2) +
                                                 LFill(VL_CRED, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI156Count := FRegistroI156Count + RegI155.RegistroI156.Count;
  end;
  Result := strRegistroI156;
end;


function TBloco_I.WriteRegistroI200: String;
var
intFor: integer;
strRegistroI200: String;
begin
  strRegistroI200 := '';

  if Assigned(FRegistroI200) then
  begin
     for intFor := 0 to FRegistroI200.Count - 1 do
     begin
        with FRegistroI200.Items[intFor] do
        begin
           ///
           strRegistroI200 :=  strRegistroI200 + LFill('I200') +
                                                 LFill(NUM_LCTO) +
                                                 LFill(DT_LCTO) +
                                                 LFill(VL_LCTO, 19, 2) +
                                                 LFill(IND_LCTO) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI200 := strRegistroI200 +
                           WriteRegistroI250(FRegistroI200.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI200;
end;

function TBloco_I.WriteRegistroI250(RegI200: TRegistroI200): String;
var
intFor: integer;
strRegistroI250: String;
begin
  strRegistroI250 := '';

  if Assigned(RegI200.RegistroI250) then
  begin
     for intFor := 0 to RegI200.RegistroI250.Count - 1 do
     begin
        with RegI200.RegistroI250.Items[intFor] do
        begin
           /// Checagem das informações que formarão o registro
           Check(((IND_DC = 'D') or (IND_DC = 'C')), '(I-I250) Indicador da natureza da partida, deve ser informado: D ou C!');
           ///
           strRegistroI250 :=  strRegistroI250 + LFill('I250') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(VL_DC, 19, 2) +
                                                 LFill(IND_DC) +
                                                 LFill(NUM_ARQ) +
                                                 LFill(COD_HIST_PAD) +
                                                 LFill(HIST) +
                                                 LFill(COD_PART) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI250 := strRegistroI250 +
                           WriteRegistroI256(RegI200.RegistroI250.Items[intFor] );


       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI250Count := FRegistroI250Count + RegI200.RegistroI250.Count;
  end;
  Result := strRegistroI250;
end;

function TBloco_I.WriteRegistroI256(RegI250: TRegistroI250): String;
var
intFor: integer;
strRegistroI256: String;
begin
  strRegistroI256 := '';

  if Assigned(RegI250.RegistroI256) then
  begin
     for intFor := 0 to RegI250.RegistroI256.Count - 1 do
     begin
        with RegI250.RegistroI256.Items[intFor] do
        begin
           strRegistroI256 :=  strRegistroI256 + LFill('I256') +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_DC, 19, 2) +
                                                 LFill(IND_DC) +
                                                 Delimitador +
                                                 #13#10;
        end;

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI256Count := FRegistroI256Count + RegI250.RegistroI256.Count;
  end;
  Result := strRegistroI256;
end;



function TBloco_I.WriteRegistroI350: String;
var
intFor: integer;
strRegistroI350: String;
begin
  strRegistroI350 := '';

  if Assigned(FRegistroI350) then
  begin
     for intFor := 0 to FRegistroI350.Count - 1 do
     begin
        with FRegistroI350.Items[intFor] do
        begin
           ///
           strRegistroI350 :=  strRegistroI350 + LFill('I350') +
                                                 LFill(DT_RES) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI350 := strRegistroI350 +
                           WriteRegistroI355(FRegistroI350.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI350;
end;

function TBloco_I.WriteRegistroI355(RegI350: TRegistroI350): String;
var
intFor: integer;
strRegistroI355: String;
begin
  strRegistroI355 := '';

  if Assigned(RegI350.RegistroI355) then
  begin
     for intFor := 0 to RegI350.RegistroI355.Count - 1 do
     begin
        with RegI350.RegistroI355.Items[intFor] do
        begin
           /// Checagem das informações que formarão o registro
           Check(((IND_DC = 'D') or (IND_DC = 'C') or (IND_DC = '')), '(I-I355) No Indicador da situação do saldo inicial, deve ser informado: D ou C ou nulo!');
           Check(((IND_DC = 'D') or (IND_DC = 'C') or (IND_DC = '')), '(I-I355) No Indicador da situação do saldo inicial, deve ser informado: D ou C ou nulo!');
           ///
           strRegistroI355 :=  strRegistroI355 + LFill('I355') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(VL_CTA, 19, 2) +
                                                 LFill(IND_DC, 0) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI355 := strRegistroI355 +
                           WriteRegistroI356(RegI350.RegistroI355.Items[intFor] );


       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI355Count := FRegistroI355Count + RegI350.RegistroI355.Count;
  end;
  Result := strRegistroI355;
end;


function TBloco_I.WriteRegistroI356(RegI355: TRegistroI355): String;
var
intFor: integer;
strRegistroI356: String;
begin
  strRegistroI356 := '';

  if Assigned(RegI355.RegistroI356) then
  begin
     for intFor := 0 to RegI355.RegistroI356.Count - 1 do
     begin
        with RegI355.RegistroI356.Items[intFor] do
        begin
           strRegistroI356 :=  strRegistroI356 + LFill('I356') +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_CTA, 19, 2) +
                                                 LFill(IND_DC, 0) +
                                                 Delimitador +
                                                 #13#10;
        end;

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI356Count := FRegistroI356Count + RegI355.RegistroI356.Count;
  end;
  Result := strRegistroI356;
end;


function TBLOCO_I.WriteRegistroI990: String;
begin
  Result := '';

  if Assigned(FRegistroI990) then
  begin
     with FRegistroI990 do
     begin
       QTD_LIN_I := QTD_LIN_I + 1;
       ///
       Result := LFill('I990') +
                 LFill(QTD_LIN_I, 0) +
                 Delimitador +
                 #13#10;
     end;
  end;
end;

end.
