{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					  Isaque Pinheiro		       }
{ 					  Daniel Simões de Almeida	       }
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

unit ACBrECFBloco_P_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_P, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  { TBloco_P }

  TBloco_P = class(TACBrSPED)
  private
    FRegistroP001: TRegistroP001;
    FRegistroP990: TRegistroP990;
    fRegistroP030Count : Integer;
    fRegistroP100Count : Integer;
    fRegistroP130Count : Integer;
    fRegistroP150Count : Integer;
    fRegistroP200Count : Integer;
    fRegistroP230Count : Integer;
    fRegistroP300Count : Integer;
    fRegistroP400Count : Integer;
    fRegistroP500Count : Integer;

    FBloco_0           : TBloco_0;

    procedure WriteRegistroP030( RegP001 : TRegistroP001 );
    procedure WriteRegistroP100( RegP030 : TRegistroP030 );
    procedure WriteRegistroP130( RegP030 : TRegistroP030 );
    procedure WriteRegistroP150( RegP030 : TRegistroP030 );
    procedure WriteRegistroP200( RegP030 : TRegistroP030 );
    procedure WriteRegistroP230( RegP030 : TRegistroP030 );
    procedure WriteRegistroP300( RegP030 : TRegistroP030 );
    procedure WriteRegistroP400( RegP030 : TRegistroP030 );
    procedure WriteRegistroP500( RegP030 : TRegistroP030 );

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create ;                                      /// Create
    destructor  Destroy; override;                            /// Destroy

    procedure LimpaRegistros;

    function RegistroP001New            : TRegistroP001;
    function RegistroP030New            : TRegistroP030;
    function RegistroP100New            : TRegistroP100;
    function RegistroP130New            : TRegistroP130;
    function RegistroP150New            : TRegistroP150;
    function RegistroP200New            : TRegistroP200;
    function RegistroP230New            : TRegistroP230;
    function RegistroP300New            : TRegistroP300;
    function RegistroP400New            : TRegistroP400;
    function RegistroP500New            : TRegistroP500;

    procedure WriteRegistroP001;
    procedure WriteRegistroP990;

    property Bloco_0           : TBloco_0      read FBloco_0           write FBloco_0;
    property RegistroP001      : TRegistroP001 read fRegistroP001      write fRegistroP001;
    property RegistroP990      : TRegistroP990 read fRegistroP990      write fRegistroP990;

    property RegistroP030Count : Integer       read FRegistroP030Count write fRegistroP030Count;
    property RegistroP100Count : Integer       read FRegistroP100Count write fRegistroP100Count;
    property RegistroP130Count : Integer       read FRegistroP130Count write fRegistroP130Count;
    property RegistroP150Count : Integer       read FRegistroP150Count write fRegistroP150Count;
    property RegistroP200Count : Integer       read FRegistroP200Count write fRegistroP200Count;
    property RegistroP230Count : Integer       read FRegistroP230Count write fRegistroP230Count;
    property RegistroP300Count : Integer       read FRegistroP300Count write fRegistroP300Count;
    property RegistroP400Count : Integer       read FRegistroP400Count write fRegistroP400Count;
    property RegistroP500Count : Integer       read FRegistroP500Count write fRegistroP500Count;

  end;

implementation

uses
  ACBrTXTUtils;

{ TBloco_P }

constructor TBloco_P.Create;
begin
  inherited;
  CriaRegistros;
end;

destructor TBloco_P.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_P.CriaRegistros;
begin
  fRegistroP001 := TRegistroP001.Create;
  fRegistroP990 := TRegistroP990.Create;

  fRegistroP030Count := 0;
  fRegistroP100Count := 0;
  fRegistroP130Count := 0;
  fRegistroP150Count := 0;
  fRegistroP200Count := 0;
  fRegistroP230Count := 0;
  fRegistroP300Count := 0;
  fRegistroP400Count := 0;
  fRegistroP500Count := 0;

  fRegistroP990.QTD_LIN_P := 0;
end;

procedure TBloco_P.LiberaRegistros;
begin
  fRegistroP001.Free;
  fRegistroP990.Free;
end;

procedure TBloco_P.LimpaRegistros;
begin
  LiberaRegistros;
  CriaRegistros;
end;

function TBloco_P.RegistroP001New: TRegistroP001;
begin
  Result := FRegistroP001;
end;

function TBloco_P.RegistroP030New: TRegistroP030;
begin
   Result := FRegistroP001.RegistroP030.New;
end;

function TBloco_P.RegistroP100New: TRegistroP100;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP100.New;
end;

function TBloco_P.RegistroP130New: TRegistroP130;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP130.New;
end;

function TBloco_P.RegistroP150New: TRegistroP150;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP150.New;
end;

function TBloco_P.RegistroP200New: TRegistroP200;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP200.New;
end;

function TBloco_P.RegistroP230New: TRegistroP230;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP230.New;
end;

function TBloco_P.RegistroP300New: TRegistroP300;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP300;
end;

function TBloco_P.RegistroP400New: TRegistroP400;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP400;
end;

function TBloco_P.RegistroP500New: TRegistroP500;
var
  P030Count: integer;
begin
  P030Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P030Count].RegistroP500;
end;

procedure TBloco_P.WriteRegistroP001;
begin
  if Assigned( fRegistroP001 ) then
  begin
    with fRegistroP001 do
    begin
       Add(LFill('P001') +
           LFill( Integer(IND_DAD), 0 ) );

       if IND_DAD = idComDados then
       begin
         WriteRegistroP030(fRegistroP001);
       end;
    end;
    RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
  end;
end;

procedure TBloco_P.WriteRegistroP030(RegP001: TRegistroP001);
var
  iCount: Integer;
begin
  if Assigned( RegP001.RegistroP030 ) then
  begin
    for iCount := 0 to RegP001.RegistroP030.Count -1 do
    begin
      with RegP001.RegistroP030.Items[iCount] do
      begin
        Add(
           LFill( 'P030' )
         );
      end;
      // Registros FILHOS
      WriteRegistroP100( RegP001.RegistroP030.Items[iCount] );
      WriteRegistroP130( RegP001.RegistroP030.Items[iCount] );
      WriteRegistroP150( RegP001.RegistroP030.Items[iCount] );
      WriteRegistroP200( RegP001.RegistroP030.Items[iCount] );
      WriteRegistroP230( RegP001.RegistroP030.Items[iCount] );
      WriteRegistroP300( RegP001.RegistroP030.Items[iCount] );
      WriteRegistroP400( RegP001.RegistroP030.Items[iCount] );
      WriteRegistroP500( RegP001.RegistroP030.Items[iCount] );

      RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
    end;
    fRegistroP030Count := fRegistroP030Count + RegP001.RegistroP030.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP100(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP100 ) then
  begin
     for iCount := 0 to RegP030.RegistroP100.Count - 1 do
     begin
        with RegP030.RegistroP100.Items[iCount] do
        begin

        end;
        RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP100Count := FRegistroP100Count + RegP030.RegistroP100.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP130(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP130 ) then
  begin
     for iCount := 0 to RegP030.RegistroP130.Count - 1 do
     begin
        with RegP030.RegistroP130.Items[iCount] do
        begin

        end;
        RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP130Count := FRegistroP130Count + RegP030.RegistroP130.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP150(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP150 ) then
  begin
     for iCount := 0 to RegP030.RegistroP150.Count - 1 do
     begin
        with RegP030.RegistroP150.Items[iCount] do
        begin

        end;
        RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP150Count := FRegistroP150Count + RegP030.RegistroP150.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP200(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP200 ) then
  begin
     for iCount := 0 to RegP030.RegistroP200.Count - 1 do
     begin
        with RegP030.RegistroP200.Items[iCount] do
        begin

        end;
        RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP200Count := FRegistroP200Count + RegP030.RegistroP200.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP230(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP230 ) then
  begin
     for iCount := 0 to RegP030.RegistroP230.Count - 1 do
     begin
        with RegP030.RegistroP230.Items[iCount] do
        begin

        end;
        RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP230Count := FRegistroP230Count + RegP030.RegistroP230.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP300(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP300 ) then
  begin
    with RegP030.RegistroP300 do
    begin

    end;
    RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
    FRegistroP300Count := FRegistroP300Count + 1;
  end;
end;

procedure TBloco_P.WriteRegistroP400(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP400 ) then
  begin
    with RegP030.RegistroP400 do
    begin

    end;
    RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
    FRegistroP400Count := FRegistroP400Count + 1;
  end;
end;

procedure TBloco_P.WriteRegistroP500(RegP030: TRegistroP030);
var
  iCount:Integer;
begin
  if Assigned( RegP030.RegistroP500 ) then
  begin
    with RegP030.RegistroP500 do
    begin

    end;
    RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
    FRegistroP500Count := FRegistroP500Count + 1;
  end;
end;

procedure TBloco_P.WriteRegistroP990;
begin
  if Assigned(RegistroP990) then
  begin
     with RegistroP990 do
     begin
        QTD_LIN_P := QTD_LIN_P + 1;
        //
        Add(LFill( 'P990' ) +
            LFill( QTD_LIN_P, 0 ) );
     end;
  end;
end;

end.
