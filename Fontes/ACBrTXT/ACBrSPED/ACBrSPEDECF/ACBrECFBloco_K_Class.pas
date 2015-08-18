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

unit ACBrECFBloco_K_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_K, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_K -

  { TBloco_K }

  TBloco_K = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroK001: TRegistroK001;
    FRegistroK030: TRegistroK030List;  /// BLOCO I - Lista de RegistroJ050
    FRegistroK990: TRegistroK990;

    FRegistroK155Count: Integer;
    FRegistroK156Count: Integer;
    FRegistroK355Count: Integer;
    FRegistroK356Count: Integer;

    function WriteRegistroK155(RegK030: TRegistroK030): String;
    function WriteRegistroK156(RegK155: TRegistroK155): String;

    function WriteRegistroK355(RegK030: TRegistroK030): String;
    function WriteRegistroK356(RegK355: TRegistroK355): String;

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    function WriteRegistroK001: String;
    function WriteRegistroK030: String;
    function WriteRegistrok990: String;

    constructor Create;
    destructor Destroy;
    procedure LimpaRegistros;

    property RegistroK001: TRegistroK001     read FRegistroK001 write FRegistroK001;
    property RegistroK030: TRegistroK030List read FRegistroK030;
    property RegistroK990: TRegistroK990     read FRegistroK990 write FRegistroK990;

    property RegistroK155Count: Integer read FRegistroK155Count write FRegistroK155Count;
    property RegistroK156Count: Integer read FRegistroK156Count write FRegistroK156Count;
    property RegistroK355Count: Integer read FRegistroK355Count write FRegistroK355Count;
    property RegistroK356Count: Integer read FRegistroK356Count write FRegistroK356Count;
  published
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_K }

constructor TBloco_K.Create;
begin
  FRegistroK001 := TRegistroK001.Create;
  FRegistroK030 := TRegistroK030List.Create;
  FRegistroK990 := TRegistroK990.Create;

  FRegistroK155Count := 0;
  FRegistroK156Count := 0;
  FRegistroK355Count := 0;
  FRegistroK356Count := 0;
end;

procedure TBloco_K.CriaRegistros;
begin
   inherited;

   FRegistroK001 := TRegistroK001.Create;
   FRegistroK030 := TRegistroK030List.Create;
   FRegistroK990 := TRegistroK990.Create;
   FRegistrok990.QTD_LIN := 0;

   FRegistroK155Count := 0;
   FRegistroK156Count := 0;
   FRegistroK355Count := 0;
   FRegistroK356Count := 0;
end;

destructor TBloco_K.Destroy;
begin
   FRegistroK001.Free;
   FRegistroK030.Free;
   FRegistroK990.Free;

   inherited;
end;

procedure TBloco_K.LiberaRegistros;
begin
   inherited;

   FRegistroK001.Free;
   FRegistroK030.Free;
   FRegistroK990.Free;
end;

procedure TBloco_K.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_K.WriteRegistroK001: String;
begin
  Result := '';

  if Assigned(FRegistroK001) then
  begin
     with FRegistroK001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(K-K001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('K001') +
                 LFill( Integer(IND_DAD), 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroK990.QTD_LIN:= FRegistroK990.QTD_LIN + 1;
     end;
  end;

end;

function TBloco_K.WriteRegistroK030: String;
var
intFor: integer;
strRegistroK030: String;
begin
  strRegistroK030 := '';

  if Assigned(FRegistroK030) then
  begin
     for intFor := 0 to FRegistroK030.Count - 1 do
     begin
        with FRegistroK030.Items[intFor] do
        begin
           ///
           strRegistroK030 :=  strRegistroK030 + LFill('K030') +
                                                 LFill(DT_INI) +
                                                 LFill(DT_FIN) +
                                                 LFill(PER_APUR) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registros Filhos
        strRegistroK030 := strRegistroK030 +
                           WriteRegistroK155(FRegistroK030.Items[intFor]) +
                           WriteRegistroK355(FRegistroK030.Items[intFor]);

       FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
     end;
  end;

  Result := strRegistroK030;
end;

function TBloco_K.WriteRegistroK155(RegK030: TRegistroK030): String;
var
intFor: integer;
strRegistroK155: String;
begin
  strRegistroK155 := '';

  if Assigned(RegK030.RegistroK155) then
  begin
     for intFor := 0 to RegK030.RegistroK155.Count - 1 do
     begin
        with RegK030.RegistroK155.Items[intFor] do
        begin
           ///
           strRegistroK155 :=  strRegistroK155 + LFill('K155') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(VL_SLD_INI, 19, 2) +
                                                 LFill(IND_VL_SLD_INI,0) +
                                                 LFill(VL_DEB, 19, 2) +
                                                 LFill(VL_CRED, 19, 2) +
                                                 LFill(VL_SLD_FIN, 19, 2) +
                                                 LFill(IND_VL_SLD_FIN,0) +
                                                 Delimitador +
                                                 #13#10;
        end;

        // Registros Filhos
        strRegistroK155 := strRegistroK155 +
                           WriteRegistroK156(RegK030.RegistroK155.Items[intFor]);

        FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
     end;

     FRegistroK155Count := FRegistroK155Count + RegK030.RegistroK155.Count;
  end;

  Result := strRegistroK155;
end;

function TBloco_K.WriteRegistroK156(RegK155: TRegistroK155): String;
var
intFor: integer;
strRegistroK156: String;
begin
  strRegistroK156 := '';

  if Assigned(RegK155.RegistroK156) then
  begin
     for intFor := 0 to RegK155.RegistroK156.Count - 1 do
     begin
        with RegK155.RegistroK156.Items[intFor] do
        begin
           ///
           strRegistroK156 :=  strRegistroK156 + LFill('K156') +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_SLD_FIN, 19, 2) +
                                                 LFill(IND_VL_SLD_FIN,0) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
     end;

     FRegistroK156Count := FRegistroK156Count + RegK155.RegistroK156.Count;
  end;

  Result := strRegistroK156;
end;

function TBloco_K.WriteRegistroK355(RegK030: TRegistroK030): String;
var
intFor: integer;
strRegistrok355: String;
begin
  strRegistrok355 := '';

  if Assigned(RegK030.Registrok355) then
  begin
     for intFor := 0 to RegK030.Registrok355.Count - 1 do
     begin
        with RegK030.Registrok355.Items[intFor] do
        begin
           ///
           strRegistrok355 :=  strRegistrok355 + LFill('k355') +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CCUS) +
                                                 LFill(VL_SLD_FIN, 19, 2) +
                                                 LFill(IND_VL_SLD_FIN,0) +
                                                 Delimitador +
                                                 #13#10;
        end;

        // Registros Filhos
        strRegistrok355 := strRegistrok355 +
                           WriteRegistroK356(RegK030.Registrok355.Items[intFor]);

        FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
     end;

     FRegistrok355Count := FRegistrok355Count + RegK030.Registrok355.Count;
  end;

  Result := strRegistrok355;
end;

function TBloco_K.WriteRegistroK356(RegK355: TRegistroK355): String;
var
intFor: integer;
strRegistroK356: String;
begin
  strRegistroK356 := '';

  if Assigned(RegK355.RegistroK356) then
  begin
     for intFor := 0 to RegK355.RegistroK356.Count - 1 do
     begin
        with RegK355.RegistroK356.Items[intFor] do
        begin
           ///
           strRegistroK356 :=  strRegistroK356 + LFill('K356') +
                                                 LFill(COD_CTA_REF) +
                                                 LFill(VL_SLD_FIN, 19, 2) +
                                                 LFill(IND_VL_SLD_FIN,0) +
                                                 Delimitador +
                                                 #13#10;
        end;

        FRegistroK990.QTD_LIN := FRegistroK990.QTD_LIN + 1;
     end;

     FRegistroK356Count := FRegistroK356Count + RegK355.RegistroK356.Count;
  end;

  Result := strRegistroK356;

end;

function TBloco_K.WriteRegistrok990: String;
begin
  Result := '';

  if Assigned(FRegistroK990) then
  begin
     with FRegistroK990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Result := LFill('K990') +
                 LFill(QTD_LIN, 0) +
                 Delimitador +
                 #13#10;
     end;
  end;
end;

end.
