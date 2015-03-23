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
|* 06/05/2014: Francinaldo A. da Costa
|*  - Modificações para o layout 2
|* 04/03/2015: Flavio Rubens Massaro Jr.
|* - Modificação para contemplar layout 3 referente ao ano calendario 2014
*******************************************************************************}

unit ACBrECDBloco_I_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrECDBloco_I;

type
  /// TBLOCO_I -
  TBLOCO_I = class(TACBrSPED)
  private
    FRegistroI001: TRegistroI001;      /// BLOCO I - RegistroI001
    FRegistroI010: TRegistroI010;      /// BLOCO I - RegistroI010
    FRegistroI012: TRegistroI012List;  /// BLOCO I - Lista de RegistroI012
    //FRegistroI015: TRegistroI015List;  /// BLOCO I - Lista de RegistroI015
    FRegistroI020: TRegistroI020List;  /// BLOCO I - Lista de RegistroI020
    FRegistroI030: TRegistroI030;      /// BLOCO I - RegistroI030
    FRegistroI050: TRegistroI050List;  /// BLOCO I - Lista de RegistroI050
    FRegistroI075: TRegistroI075List;  /// BLOCO I - Lista de RegistroI075
    FRegistroI100: TRegistroI100List;  /// BLOCO I - Lista de RegistroI100
    FRegistroI150: TRegistroI150List;  /// BLOCO I - Lista de RegistroI150
    FRegistroI200: TRegistroI200List;
    FRegistroI300: TRegistroI300List;
    FRegistroI350: TRegistroI350List;  /// BLOCO I - Lista de RegistroI350
    FRegistroI500: TRegistroI500List;  /// BLOCO I - Lista de RegistroI500
    FRegistroI510: TRegistroI510List;  /// BLOCO I - Lista de RegistroI510
    FRegistroI550: TRegistroI550List;
    FRegistroI990: TRegistroI990;      /// BLOCO I - FRegistroI990

    FRegistroI015Count: Integer;
    FRegistroI051Count: Integer;
    FRegistroI052Count: Integer;
    FRegistroI053Count: Integer;
    FRegistroI151Count: Integer;
    FRegistroI155Count: Integer;
    FRegistroI250Count: Integer;
    FRegistroI310Count: Integer;
    FRegistroI355Count: Integer;
    FRegistroI555Count: Integer;

    function WriteRegistroI015(RegI012: TRegistroI012): String;
    function WriteRegistroI051(RegI050: TRegistroI050): String;
    function WriteRegistroI052(RegI050: TRegistroI050): String;
    function WriteRegistroI053(RegI050: TRegistroI050): String;
    function WriteRegistroI151(RegI150: TRegistroI150): String;
    function WriteRegistroI155(RegI150: TRegistroI150): String;
    function WriteRegistroI250(RegI200: TRegistroI200): String;
    function WriteRegistroI310(RegI300: TRegistroI300): String;
    function WriteRegistroI355(RegI350: TRegistroI350): String;
    function WriteRegistroI555(RegI550: TRegistroI550): String;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function WriteRegistroI001: String;
    function WriteRegistroI010: String;
    function WriteRegistroI012: String;
    function WriteRegistroI020: String;
    function WriteRegistroI030: String;
    function WriteRegistroI050: String;
    function WriteRegistroI075: String;
    function WriteRegistroI100: String;
    function WriteRegistroI150: String;
    function WriteRegistroI200: String;
    function WriteRegistroI300: String;
    function WriteRegistroI350: String;
    function WriteRegistroI500: String;
    function WriteRegistroI510: String;
    function WriteRegistroI550: String;
    function WriteRegistroI990: String;

    property RegistroI001: TRegistroI001     read FRegistroI001 write FRegistroI001;
    property RegistroI010: TRegistroI010     read FRegistroI010 write FRegistroI010;
    property RegistroI012: TRegistroI012List read fRegistroI012 write fRegistroI012;
    //property RegistroI015: TRegistroI015List read fRegistroI015 write fRegistroI015;
    property RegistroI020: TRegistroI020List read fRegistroI020 write fRegistroI020;
    property RegistroI030: TRegistroI030     read fRegistroI030 write fRegistroI030;
    property RegistroI050: TRegistroI050List read fRegistroI050 write fRegistroI050;
    property RegistroI075: TRegistroI075List read fRegistroI075 write fRegistroI075;
    property RegistroI100: TRegistroI100List read fRegistroI100 write fRegistroI100;
    property RegistroI150: TRegistroI150List read fRegistroI150 write fRegistroI150;
    property RegistroI200: TRegistroI200List read fRegistroI200 write fRegistroI200;
    property RegistroI300: TRegistroI300List read fRegistroI300 write fRegistroI300;
    property RegistroI350: TRegistroI350List read fRegistroI350 write fRegistroI350;
    property RegistroI500: TRegistroI500List read fRegistroI500 write fRegistroI500;
    property RegistroI510: TRegistroI510List read fRegistroI510 write fRegistroI510;
    property RegistroI550: TRegistroI550List read fRegistroI550 write fRegistroI550;
    property RegistroI990: TRegistroI990     read FRegistroI990 write FRegistroI990;

    property RegistroI015Count: Integer read FRegistroI015Count write FRegistroI015Count;
    property RegistroI051Count: Integer read FRegistroI051Count write FRegistroI051Count;
    property RegistroI052Count: Integer read FRegistroI052Count write FRegistroI052Count;
    property RegistroI053Count: Integer read FRegistroI053Count write FRegistroI053Count;    
    property RegistroI151Count: Integer read FRegistroI151Count write FRegistroI151Count;
    property RegistroI155Count: Integer read FRegistroI155Count write FRegistroI155Count;
    property RegistroI250Count: Integer read FRegistroI250Count write FRegistroI250Count;
    property RegistroI310Count: Integer read FRegistroI310Count write FRegistroI310Count;
    property RegistroI355Count: Integer read FRegistroI355Count write FRegistroI355Count;
    property RegistroI555Count: Integer read FRegistroI555Count write FRegistroI555Count;
  end;

implementation

{ TBLOCO_I }

constructor TBLOCO_I.Create;
begin
  FRegistroI001 := TRegistroI001.Create;
  FRegistroI010 := TRegistroI010.Create;
  FRegistroI012 := TRegistroI012List.Create;
  FRegistroI020 := TRegistroI020List.Create;
  FRegistroI030 := TRegistroI030.Create;
  FRegistroI050 := TRegistroI050List.Create;
  FRegistroI075 := TRegistroI075List.Create;
  FRegistroI100 := TRegistroI100List.Create;
  FRegistroI150 := TRegistroI150List.Create;
  FRegistroI200 := TRegistroI200List.Create;
  FRegistroI300 := TRegistroI300List.Create;
  FRegistroI350 := TRegistroI350List.Create;
  FRegistroI500 := TRegistroI500List.Create;
  FRegistroI510 := TRegistroI510List.Create;
  FRegistroI550 := TRegistroI550List.Create;
  FRegistroI990 := TRegistroI990.Create;

  FRegistroI015Count := 0;
  FRegistroI051Count := 0;
  FRegistroI052Count := 0;
  FRegistroI053Count := 0;
  FRegistroI151Count := 0;
  FRegistroI155Count := 0;
  FRegistroI250Count := 0;
  FRegistroI310Count := 0;
  FRegistroI355Count := 0;
  FRegistroI555Count := 0;

  FRegistroI990.QTD_LIN_I := 0;
end;

destructor TBLOCO_I.Destroy;
begin
  FRegistroI001.Free;
  FRegistroI010.Free;
  FRegistroI012.Free;
  FRegistroI020.Free;
  FRegistroI030.Free;
  FRegistroI050.Free;
  FRegistroI075.Free;
  FRegistroI100.Free;
  FRegistroI150.Free;
  FRegistroI200.Free;
  FRegistroI300.Free;
  FRegistroI350.Free;
  FRegistroI500.Free;
  FRegistroI510.Free;
  FRegistroI550.Free;

  FRegistroI990.Free;
  inherited;
end;

procedure TBLOCO_I.LimpaRegistros;
begin
  FRegistroI012.Clear;
  FRegistroI020.Clear;
  FRegistroI050.Clear;
  FRegistroI075.Clear;
  FRegistroI100.Clear;
  FRegistroI150.Clear;
  FRegistroI200.Clear;
  FRegistroI300.Clear;
  FRegistroI350.Clear;
  FRegistroI510.Clear;
  FRegistroI550.Clear;

  FRegistroI015Count := 0;
  FRegistroI051Count := 0;
  FRegistroI052Count := 0;
  FRegistroI053Count := 0;  
  FRegistroI151Count := 0;
  FRegistroI155Count := 0;
  FRegistroI250Count := 0;
  FRegistroI310Count := 0;
  FRegistroI355Count := 0;
  FRegistroI555Count := 0;

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

function TBLOCO_I.WriteRegistroI010: String;
begin
  Result := '';

  if Assigned(FRegistroI010) then
  begin
     with FRegistroI010 do
     begin
       /// Checagem das informações que formarão o registro
       ///
       if DT_INI >= EncodeDate(2014,01,01) then
         Check(((IND_ESC = 'G') or (IND_ESC = 'R') or (IND_ESC = 'A') or (IND_ESC = 'B') or (IND_ESC = 'Z') or (IND_ESC = 'S')), '(I-I010) No Indicador da forma de escrituração contábil, deve ser informado: G ou R ou A ou B ou Z ou S!')
       else
         Check(((IND_ESC = 'G') or (IND_ESC = 'R') or (IND_ESC = 'A') or (IND_ESC = 'B') or (IND_ESC = 'Z')), '(I-I010) No Indicador da forma de escrituração contábil, deve ser informado: G ou R ou A ou B ou Z!');
       ///
       Result := LFill('I010') +
                 LFill(IND_ESC, 1) +
                 LFill(COD_VER_LC) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
end;

function TBloco_I.WriteRegistroI012: String;
var
intFor: integer;
strRegistroI012: String;
begin
  strRegistroI012 := '';

  if Assigned(FRegistroI012) then
  begin
     for intFor := 0 to FRegistroI012.Count - 1 do
     begin
       with FRegistroI012.Items[intFor] do
       begin
          /// Checagem das informações que formarão o registro
          Check(((TIPO = '0') or (TIPO = '1')), '(I-I012) No Tipo de Escrituração do livro, deve ser informado: 0 ou 1!');
          ///
          strRegistroI012 :=  strRegistroI012 + LFill('I012') +
                                                LFill(NUM_ORD) +
                                                LFill(NAT_LIVR) +
                                                LFill(TIPO, 1) +
                                                RFill(COD_HASH_AUX,40) +
                                                Delimitador +
                                                #13#10;
       end;
       // Registros Filhos
       strRegistroI012 := strRegistroI012 +
                          WriteRegistroI015(FRegistroI012.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI012;
end;

function TBloco_I.WriteRegistroI015(RegI012: TRegistroI012): String;
var
intFor: integer;
strRegistroI015: String;
begin
  strRegistroI015 := '';

  if Assigned(RegI012.RegistroI015) then
  begin
     for intFor := 0 to RegI012.RegistroI015.Count - 1 do
     begin
        with RegI012.RegistroI015.Items[intFor] do
        begin
           ///
           strRegistroI015 :=  strRegistroI015 + LFill('I015') +
                                                 LFill(COD_CTA_RES) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI015Count := FRegistroI015Count + RegI012.RegistroI015.Count;
  end;
  Result := strRegistroI015;
end;

function TBloco_I.WriteRegistroI020: String;
var
intFor: integer;
strRegistroI020: String;
begin
  strRegistroI020 := '';

  if Assigned(FRegistroI020) then
  begin
     for intFor := 0 to FRegistroI020.Count - 1 do
     begin
        with FRegistroI020.Items[intFor] do
        begin
           /// Checagem das informações que formarão o registro
           Check(CAMPO <> '', '(I-I020) O nome do campo adicional é obrigatório!');
           Check(((TIPO_DADO = 'N') or (TIPO_DADO = 'C')), '(I-I020) Na Indicação do tipo de dado, deve ser informado: N ou C!');
           ///
           strRegistroI020 :=  strRegistroI020 + LFill('I020') +
                                                 LFill(REG_COD, 4) +
                                                 LFill(NUM_AD) +
                                                 LFill(CAMPO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(TIPO_DADO, 1) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI020;
end;

function TBLOCO_I.WriteRegistroI030: String;
begin
  Result := '';

  if Assigned(FRegistroI030) then
  begin
     with FRegistroI030 do
     begin
       /// Layout 3 a partir da escrituração ano calendário 2014
       if DT_INI >= EncodeDate(2014,01,01) then
         begin
           Result := LFill('I030') +
                     LFill('TERMO DE ABERTURA') +
                     LFill(NUM_ORD) +
                     LFill(NAT_LIVR) +
                     LFill('[*******]') +
                     LFill(NOME) +
                     LFill(NIRE) +
                     LFill(CNPJ) +
                     LFill(DT_ARQ) +
                     LFill(DT_ARQ_CONV, 'ddmmyyyy' ) +
                     LFill(DESC_MUN) +
                     LFill(DT_EX_SOCIAL, 'ddmmyyyy' ) +
                     Delimitador +
                     #13#10;
         end
       /// Layout 2 a partir da escrituração ano calendário 2013
       else if DT_INI >= EncodeDate(2013,01,01) then
         begin
           Result := LFill('I030') +
                     LFill('TERMO DE ABERTURA') +
                     LFill(NUM_ORD) +
                     LFill(NAT_LIVR) +
                     LFill('[*******]') +
                     LFill(NOME) +
                     LFill(NIRE, 11) +
                     LFill(CNPJ) +
                     LFill(DT_ARQ) +
                     LFill(DT_ARQ_CONV, 'ddmmyyyy' ) +
                     LFill(DESC_MUN) +
                     LFill(DT_EX_SOCIAL, 'ddmmyyyy' ) +
                     LFill(NOME_AUDITOR) +
                     LFill(COD_CVM_AUDITOR) +
                     Delimitador +
                     #13#10;
         end
       else
         begin
           Result := LFill('I030') +
                     LFill('TERMO DE ABERTURA') +
                     LFill(NUM_ORD) +
                     LFill(NAT_LIVR) +
                     LFill('[*******]') +
                     LFill(NOME) +
                     LFill(NIRE, 11) +
                     LFill(CNPJ) +
                     LFill(DT_ARQ) +
                     LFill(DT_ARQ_CONV, 'ddmmyyyy' ) +
                     LFill(DESC_MUN) +
                     Delimitador +
                     #13#10;
         end;
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
                           WriteRegistroI051(FRegistroI050.Items[intFor] ) +
                           WriteRegistroI052(FRegistroI050.Items[intFor] ) +
                           WriteRegistroI053(FRegistroI050.Items[intFor] );

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

function TBloco_I.WriteRegistroI052(RegI050: TRegistroI050): String;
var
intFor: integer;
strRegistroI052: String;
begin
  strRegistroI052 := '';

  if Assigned(RegI050.RegistroI052) then
  begin
     for intFor := 0 to RegI050.RegistroI052.Count - 1 do
     begin
        with RegI050.RegistroI052.Items[intFor] do
        begin
           ///
           strRegistroI052 :=  strRegistroI052 + LFill('I052') +
                                                 LFill(COD_CCUS) +
                                                 LFill(COD_AGL) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI052Count := FRegistroI052Count + RegI050.RegistroI052.Count;
  end;
  Result := strRegistroI052;
end;

function TBloco_I.WriteRegistroI053(RegI050: TRegistroI050): String;
var
intFor: integer;
strRegistroI053: String;
begin
  strRegistroI053 := '';

  if Assigned(RegI050.RegistroI053) then
  begin
     for intFor := 0 to RegI050.RegistroI053.Count - 1 do
     begin
        with RegI050.RegistroI053.Items[intFor] do
        begin
           ///
           strRegistroI053 :=  strRegistroI053 + LFill('I053') +
                                                 LFill(COD_IDT) +
                                                 LFill(COD_CNT_CORR) +
                                                 LFill(NAT_SUB_CNT) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI053Count := FRegistroI053Count + RegI050.RegistroI053.Count;
  end;
  Result := strRegistroI053;
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
                           WriteRegistroI151(RegistroI150.Items[intFor] ) +
                           WriteRegistroI155(RegistroI150.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI150;
end;

function TBloco_I.WriteRegistroI151(RegI150: TRegistroI150): String;
var
intFor: integer;
strRegistroI151: String;
begin
  strRegistroI151 := '';

  if Assigned(RegI150.RegistroI151) then
  begin
     for intFor := 0 to RegI150.RegistroI151.Count - 1 do
     begin
        with RegI150.RegistroI151.Items[intFor] do
        begin
           ///
           strRegistroI151 :=  strRegistroI151 + LFill('I151') +
                                                 LFill(ASSIM_DIG) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI151Count := FRegistroI151Count + RegI150.RegistroI151.Count;
  end;
  Result := strRegistroI151;
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
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI155Count := FRegistroI155Count + RegI150.RegistroI155.Count;
  end;
  Result := strRegistroI155;
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
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI250Count := FRegistroI250Count + RegI200.RegistroI250.Count;
  end;
  Result := strRegistroI250;
end;


function TBloco_I.WriteRegistroI300: String;
var
intFor: integer;
strRegistroI300: String;
begin
  strRegistroI300 := '';

  if Assigned(FRegistroI300) then
  begin
     for intFor := 0 to FRegistroI300.Count - 1 do
     begin
        with FRegistroI300.Items[intFor] do
        begin
           ///
           strRegistroI300 :=  strRegistroI300 + LFill('I300') +
                                                 LFill(DT_BCTE) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI300 := strRegistroI300 +
                           WriteRegistroI310(FRegistroI300.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI300;
end;

function TBloco_I.WriteRegistroI310(RegI300: TRegistroI300): String;
var
intFor: integer;
strRegistroI310: String;
begin
  strRegistroI310 := '';

  if Assigned(RegI300.RegistroI310) then
  begin
     for intFor := 0 to RegI300.RegistroI310.Count - 1 do
     begin
        with RegI300.RegistroI310.Items[intFor] do
        begin
           ///
           strRegistroI310 :=  strRegistroI310 + LFill('I310') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(VAL_DEBD, 19, 2) +
                                                 LFill(VAL_CRED, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI310Count := FRegistroI310Count + RegI300.RegistroI310.Count;
  end;
  Result := strRegistroI310;
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
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI355Count := FRegistroI355Count + RegI350.RegistroI355.Count;
  end;
  Result := strRegistroI355;
end;

function TBLOCO_I.WriteRegistroI500: String;
var
intFor: integer;
strRegistroI500: String;
begin
  strRegistroI500 := '';

  if Assigned(FRegistroI500) then
  begin
     for intFor := 0 to FRegistroI500.Count - 1 do
     begin
        with FRegistroI500.Items[intFor] do
        begin
           ///
           strRegistroI500 := strRegistroI500 + LFill('I500') +
                                                LFill(TAM_FONTE, 2) +
                                                Delimitador +
                                                #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI500;
end;


function TBloco_I.WriteRegistroI510: String;
var
intFor: integer;
strRegistroI510: String;
begin
  strRegistroI510 := '';

  if Assigned(FRegistroI510) then
  begin
     for intFor := 0 to FRegistroI510.Count - 1 do
     begin
        with FRegistroI510.Items[intFor] do
        begin
           ///
           strRegistroI510 :=  strRegistroI510 + LFill('I510') +
                                                 RFill(NM_CAMPO,16,' ') +
                                                 RFill(DESC_CAMPO, 50, ' ') +
                                                 LFill(TIPO_CAMPO) +
                                                 LFill(TAM_CAMPO, 3) +
                                                 LFill(DEC_CAMPO, 2) +
                                                 LFill(COL_CAMPO, 3) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI510;
end;

function TBloco_I.WriteRegistroI550: String;
var
intFor: integer;
strRegistroI550: String;
begin
  strRegistroI550 := '';

  if Assigned(FRegistroI550) then
  begin
     for intFor := 0 to FRegistroI550.Count - 1 do
     begin
        with FRegistroI550.Items[intFor] do
        begin
           ///
           strRegistroI550 :=  strRegistroI550 + LFill('I550') +
                                                 RFill(RZ_CONT) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registro Filho
        strRegistroI550 := strRegistroI550 +
                           WriteRegistroI555(FRegistroI550.Items[intFor] );

       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
  end;
  Result := strRegistroI550;
end;

function TBloco_I.WriteRegistroI555(RegI550: TRegistroI550): String;
var
intFor: integer;
strRegistroI555: String;
begin
  strRegistroI555 := '';

  if Assigned(RegI550.RegistroI555) then
  begin
     for intFor := 0 to RegI550.RegistroI555.Count - 1 do
     begin
        with RegI550.RegistroI555.Items[intFor] do
        begin
           ///
           strRegistroI555 :=  strRegistroI555 + LFill('I555') +
                                                 RFill(RZ_CONT_TOT) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroI990.QTD_LIN_I := FRegistroI990.QTD_LIN_I + 1;
     end;
     FRegistroI555Count := FRegistroI555Count + RegI550.RegistroI555.Count;
  end;
  Result := strRegistroI555;
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
