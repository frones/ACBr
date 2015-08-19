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

    procedure WriteRegistroN500(RegN030: TRegistroN030);
    procedure WriteRegistroN600(RegN030: TRegistroN030);
    procedure WriteRegistroN610(RegN030: TRegistroN030);
    procedure WriteRegistroN615(RegN030: TRegistroN030);
    procedure WriteRegistroN620(RegN030: TRegistroN030);
    procedure WriteRegistroN630(RegN030: TRegistroN030);
    procedure WriteRegistroN650(RegN030: TRegistroN030);
    procedure WriteRegistroN660(RegN030: TRegistroN030);
    procedure WriteRegistroN670(RegN030: TRegistroN030);

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    procedure WriteRegistroN001;
    procedure WriteRegistroN030;
    procedure WriteRegistroN990;

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

procedure TBloco_N.WriteRegistroN001;
begin
  if Assigned(FRegistroN001) then
  begin
     with FRegistroN001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(N-N001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add(LFill('N001') +
           LFill( Integer(IND_DAD), 1));
       ///
       FRegistroN990.QTD_LIN:= FRegistroN990.QTD_LIN + 1;
       WriteRegistroN030;
     end;
  end;

end;

procedure TBloco_N.WriteRegistroN030;
var
intFor: integer;
begin
  if Assigned(FRegistroN030) then
  begin
     for intFor := 0 to FRegistroN030.Count - 1 do
     begin
        with FRegistroN030.Items[intFor] do
        begin
           ///
           Add(LFill('N030') +
               LFill(DT_INI) +
               LFill(DT_FIN) +
               LFill(PER_APUR));
        end;

        // Registros Filhos
        WriteRegistroN500(FRegistroN030.Items[intFor]);
        WriteRegistroN600(FRegistroN030.Items[intFor]);
        WriteRegistroN610(FRegistroN030.Items[intFor]);
        WriteRegistroN615(FRegistroN030.Items[intFor]);
        WriteRegistroN620(FRegistroN030.Items[intFor]);
        WriteRegistroN630(FRegistroN030.Items[intFor]);
        WriteRegistroN650(FRegistroN030.Items[intFor]);
        WriteRegistroN660(FRegistroN030.Items[intFor]);
        WriteRegistroN670(FRegistroN030.Items[intFor]);

       FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_N.WriteRegistroN500(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN500) then
  begin
     for intFor := 0 to RegN030.RegistroN500.Count - 1 do
     begin
        with RegN030.RegistroN500.Items[intFor] do
        begin
           ///
           Add(LFill('N500')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN500Count := FRegistroN500Count + RegN030.RegistroN500.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN600(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN600) then
  begin
     for intFor := 0 to RegN030.RegistroN600.Count - 1 do
     begin
        with RegN030.RegistroN600.Items[intFor] do
        begin
           ///
           Add(LFill('N600')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN600Count := FRegistroN600Count + RegN030.RegistroN600.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN610(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN610) then
  begin
     for intFor := 0 to RegN030.RegistroN610.Count - 1 do
     begin
        with RegN030.RegistroN610.Items[intFor] do
        begin
           ///
           Add(LFill('N610')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN610Count := FRegistroN610Count + RegN030.RegistroN610.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN615(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN615) then
  begin
     for intFor := 0 to RegN030.RegistroN615.Count - 1 do
     begin
        with RegN030.RegistroN615.Items[intFor] do
        begin
           ///
           Add(LFill('N615')                     +
              VLFill(BASE_CALC, 19, 2)           +
              VLFill(PER_INCEN_FINOR, 5, 2)      +
              VLFill(VL_LIQ_INCEN_FINOR, 19, 2)  +
              VLFill(PER_INCEN_FINAM, 5, 2)      +
              VLFill(VL_LIQ_INCEN_FINAM, 19, 2)  +
              VLFill(VL_SUBTOTAL, 19, 2)         +
              VLFill(PER_INCEN_FUNRES, 5, 2)     +
              VLFill(VL_LIQ_INCEN_FUNRES, 19, 2) +
              VLFill(VL_TOTAL, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN615Count := FRegistroN615Count + RegN030.RegistroN615.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN620(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN620) then
  begin
     for intFor := 0 to RegN030.RegistroN620.Count - 1 do
     begin
        with RegN030.RegistroN620.Items[intFor] do
        begin
           ///
           Add(LFill('N620')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN620Count := FRegistroN620Count + RegN030.RegistroN620.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN630(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN630) then
  begin
     for intFor := 0 to RegN030.RegistroN630.Count - 1 do
     begin
        with RegN030.RegistroN630.Items[intFor] do
        begin
           ///
           Add(LFill('N630')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN630Count := FRegistroN630Count + RegN030.RegistroN630.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN650(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN650) then
  begin
     for intFor := 0 to RegN030.RegistroN650.Count - 1 do
     begin
        with RegN030.RegistroN650.Items[intFor] do
        begin
           ///
           Add(LFill('N650')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN650Count := FRegistroN650Count + RegN030.RegistroN650.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN660(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN660) then
  begin
     for intFor := 0 to RegN030.RegistroN660.Count - 1 do
     begin
        with RegN030.RegistroN660.Items[intFor] do
        begin
           ///
           Add(LFill('N660')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN660Count := FRegistroN660Count + RegN030.RegistroN660.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN670(RegN030: TRegistroN030);
var
intFor: integer;
begin
  if Assigned(RegN030.RegistroN670) then
  begin
     for intFor := 0 to RegN030.RegistroN670.Count - 1 do
     begin
        with RegN030.RegistroN670.Items[intFor] do
        begin
           ///
           Add(LFill('N670')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19, 2));
        end;

        FRegistroN990.QTD_LIN := FRegistroN990.QTD_LIN + 1;
     end;

     FRegistroN670Count := FRegistroN670Count + RegN030.RegistroN670.Count;
  end;
end;

procedure TBloco_N.WriteRegistroN990;
begin
  if Assigned(FRegistroN990) then
  begin
     with FRegistroN990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Add(LFill('N990') +
           LFill(QTD_LIN, 0));
     end;
  end;
end;

end.
