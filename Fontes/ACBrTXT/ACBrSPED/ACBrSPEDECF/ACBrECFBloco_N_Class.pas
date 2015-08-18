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
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_N_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_N, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_N -

  { TBloco_N }

  TBloco_N = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroN001: TRegistroN001;
    FRegistroN990: TRegistroN990;
    FRegistroN030: TRegistroN030List;

    FRegistroN670Count: Integer;
    FRegistroN500Count: Integer;
    FRegistroN650Count: Integer;
    FRegistroN615Count: Integer;
    FRegistroN620Count: Integer;
    FRegistroN630Count: Integer;
    FRegistroN600Count: Integer;
    FRegistroN610Count: Integer;
    FRegistroN660Count: Integer;

    function WriteRegistroN500(RegN030: TRegistroN030): String;
    function WriteRegistroN600(RegN030: TRegistroN030): String;
    function WriteRegistroN610(RegN030: TRegistroN030): String;
    function WriteRegistroN615(RegN030: TRegistroN030): String;
    function WriteRegistroN620(RegN030: TRegistroN030): String;
    function WriteRegistroN630(RegN030: TRegistroN030): String;
    function WriteRegistroN650(RegN030: TRegistroN030): String;
    function WriteRegistroN660(RegN030: TRegistroN030): String;
    function WriteRegistroN670(RegN030: TRegistroN030): String;

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    function WriteRegistroN001: String;
    function WriteRegistroN030: String;
    function WriteRegistroN990: String;

    constructor Create;
    destructor Destroy;
    procedure LimpaRegistros;

    property RegistroN001: TRegistroN001     read FRegistroN001 write FRegistroN001;
    property RegistroN030: TRegistroN030List read FRegistroN030 write FregistroN030;
    property RegistroN990: TRegistroN990     read FRegistroN990 write FRegistroN990;

    property RegistroN500Count: Integer read FRegistroN500Count write FRegistroN500Count;
    property RegistroN600Count: Integer read FRegistroN600Count write FRegistroN600Count;
    property RegistroN610Count: Integer read FRegistroN610Count write FRegistroN610Count;
    property RegistroN615Count: Integer read FRegistroN615Count write FRegistroN615Count;
    property RegistroN620Count: Integer read FRegistroN620Count write FRegistroN620Count;
    property RegistroN630Count: Integer read FRegistroN630Count write FRegistroN630Count;
    property RegistroN650Count: Integer read FRegistroN650Count write FRegistroN650Count;
    property RegistroN660Count: Integer read FRegistroN660Count write FRegistroN660Count;
    property RegistroN670Count: Integer read FRegistroN670Count write FRegistroN670Count;
  published
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_N }

constructor TBloco_N.Create;
begin
   inherited;

   FRegistroN001 := TRegistroN001.Create;
   FRegistroN030 := TRegistroN030List.Create;
   FRegistroN990 := TRegistroN990.Create;

   FRegistroN670Count := 0;
   FRegistroN500Count := 0;
   FRegistroN650Count := 0;
   FRegistroN615Count := 0;
   FRegistroN620Count := 0;
   FRegistroN630Count := 0;
   FRegistroN600Count := 0;
   FRegistroN610Count := 0;
   FRegistroN660Count := 0;
end;

procedure TBloco_N.CriaRegistros;
begin
   inherited;

   FRegistroN001 := TRegistroN001.Create;
   FRegistroN030 := TRegistroN030List.Create;
   FRegistroN990 := TRegistroN990.Create;

   FRegistroN990.QTD_LIN := 0;
   FRegistroN670Count := 0;
   FRegistroN500Count := 0;
   FRegistroN650Count := 0;
   FRegistroN615Count := 0;
   FRegistroN620Count := 0;
   FRegistroN630Count := 0;
   FRegistroN600Count := 0;
   FRegistroN610Count := 0;
   FRegistroN660Count := 0;
end;

destructor TBloco_N.Destroy;
begin
  FRegistroN001.Free;
  FRegistroN030.Free;
  FRegistroN990.Free;

  inherited;
end;

procedure TBloco_N.LiberaRegistros;
begin
   inherited;

  FRegistroN001.Free;
  FRegistroN990.Free;
end;

procedure TBloco_N.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_N.WriteRegistroN001: String;
begin
  Result := '';

  if Assigned(FRegistroN001) then
  begin
     with FRegistroN001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(N-N001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('N001') +
                 LFill( Integer(IND_DAD), 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroN990.QTD_LIN:= FRegistroN990.QTD_LIN + 1;
     end;
  end;

end;

function TBloco_N.WriteRegistroN030: String;
var
intFor: integer;
strRegistroN030: String;
begin
  strRegistroN030 := '';

  if Assigned(FRegistroN030) then
  begin
     for intFor := 0 to FRegistroN030.Count - 1 do
     begin
        with FRegistroN030.Items[intFor] do
        begin
           ///
           strRegistroN030 :=  strRegistroN030 + LFill('N030') +
                                                 LFill(DT_INI) +
                                                 LFill(DT_FIN) +
                                                 LFill(PER_APUR) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registros Filhos
        strRegistroN030 := strRegistroN030 +
                           WriteRegistroN500(FRegistroN030.Items[intFor]) +
                           WriteRegistroN600(FRegistroN030.Items[intFor]) +
                           WriteRegistroN610(FRegistroN030.Items[intFor]) +
                           WriteRegistroN615(FRegistroN030.Items[intFor]) +
                           WriteRegistroN620(FRegistroN030.Items[intFor]) +
                           WriteRegistroN630(FRegistroN030.Items[intFor]) +
                           WriteRegistroN650(FRegistroN030.Items[intFor]) +
                           WriteRegistroN660(FRegistroN030.Items[intFor]) +
                           WriteRegistroN670(FRegistroN030.Items[intFor]);

       FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;
  end;

  Result := strRegistroN030;

end;

function TBloco_N.WriteRegistroN500(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN500: String;
begin
  strRegistroN500 := '';

  if Assigned(RegN030.RegistroN500) then
  begin
     for intFor := 0 to RegN030.RegistroN500.Count - 1 do
     begin
        with RegN030.RegistroN500.Items[intFor] do
        begin
           ///
           strRegistroN500 :=  strRegistroN500 + LFill('N500') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN500Count := FRegistroN500Count + RegN030.RegistroN500.Count;
  end;

  Result := strRegistroN500;
end;

function TBloco_N.WriteRegistroN600(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN600: String;
begin
  strRegistroN600 := '';

  if Assigned(RegN030.RegistroN600) then
  begin
     for intFor := 0 to RegN030.RegistroN600.Count - 1 do
     begin
        with RegN030.RegistroN600.Items[intFor] do
        begin
           ///
           strRegistroN600 :=  strRegistroN600 + LFill('N600') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN600Count := FRegistroN600Count + RegN030.RegistroN600.Count;
  end;

  Result := strRegistroN600;

end;

function TBloco_N.WriteRegistroN610(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN610: String;
begin
  strRegistroN610 := '';

  if Assigned(RegN030.RegistroN610) then
  begin
     for intFor := 0 to RegN030.RegistroN610.Count - 1 do
     begin
        with RegN030.RegistroN610.Items[intFor] do
        begin
           ///
           strRegistroN610 :=  strRegistroN610 + LFill('N610') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN610Count := FRegistroN610Count + RegN030.RegistroN610.Count;
  end;

  Result := strRegistroN610;
end;

function TBloco_N.WriteRegistroN615(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN615: String;
begin
  strRegistroN615 := '';

  if Assigned(RegN030.RegistroN615) then
  begin
     for intFor := 0 to RegN030.RegistroN615.Count - 1 do
     begin
        with RegN030.RegistroN615.Items[intFor] do
        begin
           ///
           strRegistroN615 :=  strRegistroN615 + LFill('N615') +
                                                 LFill(BASE_CALC, 19, 2) +
                                                 LFill(PER_INCEN_FINOR, 5, 2) +
                                                 LFill(VL_LIQ_INCEN_FINOR, 19, 2) +
                                                 LFill(PER_INCEN_FINAM, 5, 2) +
                                                 LFill(VL_LIQ_INCEN_FINAM, 19, 2) +
                                                 LFill(VL_SUBTOTAL, 19, 2) +
                                                 LFill(PER_INCEN_FUNRES, 5, 2) +
                                                 LFill(VL_LIQ_INCEN_FUNRES, 19, 2) +
                                                 LFill(VL_TOTAL, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN615Count := FRegistroN615Count + RegN030.RegistroN615.Count;
  end;

  Result := strRegistroN615;


end;

function TBloco_N.WriteRegistroN620(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN620: String;
begin
  strRegistroN620 := '';

  if Assigned(RegN030.RegistroN620) then
  begin
     for intFor := 0 to RegN030.RegistroN620.Count - 1 do
     begin
        with RegN030.RegistroN620.Items[intFor] do
        begin
           ///
           strRegistroN620 :=  strRegistroN620 + LFill('N620') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN620Count := FRegistroN620Count + RegN030.RegistroN620.Count;
  end;

  Result := strRegistroN620;

end;

function TBloco_N.WriteRegistroN630(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN630: String;
begin
  strRegistroN630 := '';

  if Assigned(RegN030.RegistroN630) then
  begin
     for intFor := 0 to RegN030.RegistroN630.Count - 1 do
     begin
        with RegN030.RegistroN630.Items[intFor] do
        begin
           ///
           strRegistroN630 :=  strRegistroN630 + LFill('N630') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN630Count := FRegistroN630Count + RegN030.RegistroN630.Count;
  end;

  Result := strRegistroN630;
end;

function TBloco_N.WriteRegistroN650(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN650: String;
begin
  strRegistroN650 := '';

  if Assigned(RegN030.RegistroN650) then
  begin
     for intFor := 0 to RegN030.RegistroN650.Count - 1 do
     begin
        with RegN030.RegistroN650.Items[intFor] do
        begin
           ///
           strRegistroN650 :=  strRegistroN650 + LFill('N650') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN650Count := FRegistroN650Count + RegN030.RegistroN650.Count;
  end;

  Result := strRegistroN650;

end;

function TBloco_N.WriteRegistroN660(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN660: String;
begin
  strRegistroN660 := '';

  if Assigned(RegN030.RegistroN660) then
  begin
     for intFor := 0 to RegN030.RegistroN660.Count - 1 do
     begin
        with RegN030.RegistroN660.Items[intFor] do
        begin
           ///
           strRegistroN660 :=  strRegistroN660 + LFill('N660') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN660Count := FRegistroN660Count + RegN030.RegistroN660.Count;
  end;

  Result := strRegistroN660;
end;

function TBloco_N.WriteRegistroN670(RegN030: TRegistroN030): String;
var
intFor: integer;
strRegistroN670: String;
begin
  strRegistroN670 := '';

  if Assigned(RegN030.RegistroN670) then
  begin
     for intFor := 0 to RegN030.RegistroN670.Count - 1 do
     begin
        with RegN030.RegistroN670.Items[intFor] do
        begin
           ///
           strRegistroN670 :=  strRegistroN670 + LFill('N670') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR, 19, 2) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN670Count := FRegistroN670Count + RegN030.RegistroN670.Count;
  end;

  Result := strRegistroN670;
end;

function TBloco_N.WriteRegistroN990: String;
begin
  Result := '';

  if Assigned(FRegistroN990) then
  begin
     with FRegistroN990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Result := LFill('N990') +
                 LFill(QTD_LIN, 0) +
                 Delimitador +
                 #13#10;
     end;
  end;
end;

end.
