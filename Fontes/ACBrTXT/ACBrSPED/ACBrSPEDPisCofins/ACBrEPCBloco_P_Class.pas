{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro, Jeferson Rodrigo Stefani e     }
{                              Edilson Alves de Oliveira                       }
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

unit ACBrEPCBloco_P_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrEPCBloco_P, ACBrEPCBlocos,
  ACBrEPCBloco_0_Class;

type
  TBloco_P = class( TACBrSPED )
  private
    fRegistroP001      : TRegistroP001;
    fRegistroP990      : TRegistroP990;

    fRegistroP010Count : Integer;
    fRegistroP100Count : Integer;
    fRegistroP110Count : Integer;
    fRegistroP199Count : Integer;
    fRegistroP200Count : Integer;
    fRegistroP210Count : Integer;

    FBloco_0           : TBloco_0;

    procedure WriteRegistroP010( RegP001 : TRegistroP001 );
    procedure WriteRegistroP100( RegP010 : TRegistroP010 );
    procedure WriteRegistroP110( RegP100 : TRegistroP100 );
    procedure WriteRegistroP199( RegP100 : TRegistroP100 );
    procedure WriteRegistroP200( RegP001 : TRegistroP001 );
    procedure WriteRegistroP210( RegP200 : TRegistroP200 );

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure LimpaRegistros; override;

    function RegistroP001New            : TRegistroP001;
    function RegistroP010New            : TRegistroP010;
    function RegistroP100New            : TRegistroP100;
    function RegistroP110New            : TRegistroP110;
    function RegistroP199New            : TRegistroP199;
    function RegistroP200New            : TRegistroP200;
    function RegistroP210New            : TRegistroP210;

    procedure WriteRegistroP001;
    procedure WriteRegistroP990;

    property Bloco_0           : TBloco_0      read FBloco_0           write FBloco_0;
    property RegistroP001      : TRegistroP001 read fRegistroP001      write fRegistroP001;
    property RegistroP990      : TRegistroP990 read fRegistroP990      write fRegistroP990;

    property RegistroP010Count : Integer       read FRegistroP010Count write fRegistroP010Count;
    property RegistroP100Count : Integer       read FRegistroP100Count write fRegistroP100Count;
    property RegistroP110Count : Integer       read FRegistroP110Count write fRegistroP110Count;
    property RegistroP199Count : Integer       read FRegistroP199Count write fRegistroP199Count;
    property RegistroP200Count : Integer       read FRegistroP200Count write fRegistroP200Count;
    property RegistroP210Count : Integer       read FRegistroP210Count write fRegistroP210Count;

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

  fRegistroP010Count      := 0;
  fRegistroP100Count      := 0;
  fRegistroP110Count      := 0;
  fRegistroP199Count      := 0;
  fRegistroP200Count      := 0;
  fRegistroP210Count      := 0;
  
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

function TBloco_P.RegistroP010New: TRegistroP010;
begin
  Result := FRegistroP001.RegistroP010.New;
end;

function TBloco_P.RegistroP100New: TRegistroP100;
var
P010Count: integer;
begin
   P010Count := FRegistroP001.RegistroP010.Count -1;
   //
   Result    := FRegistroP001.RegistroP010.Items[P010Count].RegistroP100.New;
end;

procedure TBloco_P.WriteRegistroP001;
begin
  if Assigned( fRegistroP001 ) then
  begin
    with fRegistroP001 do
    begin
       Add(LFill('P001') +
           LFill( Integer(IND_MOV), 0 ) );

       if IND_MOV = imComDados then
       begin
         WriteRegistroP010(fRegistroP001);
         WriteRegistroP200(fRegistroP001);
       end;
    end;
    RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
  end;
end;

procedure TBloco_P.WriteRegistroP010(RegP001: TRegistroP001);
var
  iCount: Integer;
begin
  if Assigned( RegP001.RegistroP010 ) then
  begin
    for iCount := 0 to RegP001.RegistroP010.Count -1 do
    begin
      with RegP001.RegistroP010.Items[iCount] do
      begin
        Check( funChecaCNPJ(CNPJ), '(P-010) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ] );
        Add(
           LFill( 'P010' ) +
           LFill( CNPJ, 14 )
         );
      end;
      // Registros FILHOS
      WriteRegistroP100( RegP001.RegistroP010.Items[iCount] );

      RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
    end;
    fRegistroP010Count := fRegistroP010Count + RegP001.RegistroP010.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP100(RegP010: TRegistroP010);
var
  iCount:Integer;
begin
  if Assigned( RegP010.RegistroP100 ) then
  begin
     for iCount := 0 to RegP010.RegistroP100.Count - 1 do
     begin
        with RegP010.RegistroP100.Items[iCount] do
        begin
          Add( LFill('P100')                               +
               LFill( DT_INI, 'ddmmyyyy' )                 +
               LFill( DT_FIM, 'ddmmyyyy' )                 +
               LFill( VL_REC_TOT_EST,0, 2 )                +
               LFill( COD_ATIV_ECON )                      +
               LFill( VL_REC_ATIV_ESTAB,0, 2 )             +
               LFill( VL_EXC,0, 2 )                        +
               LFill( VL_BC_CONT,0, 2 )                    +
               LFill( ALIQ_CONT,0,4  )                     +
               LFill( VL_CONT_APU,0, 2 )                   +
               LFill( COD_CTA )                            +
               LFill( INFO_COMPL  ) ) ;
        end;
        // Registros FILHOS
        WriteRegistroP110( RegP010.RegistroP100.Items[iCount] );
        WriteRegistroP199( RegP010.RegistroP100.Items[iCount] );

        RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroP100Count := FRegistroP100Count + RegP010.RegistroP100.Count;
  end;
end;

(*By: Edilson Alves de Oliveira*)
procedure TBloco_P.WriteRegistroP200(RegP001: TRegistroP001);
var
  iCount:Integer;
begin
  if Assigned( RegP001.RegistroP200 ) then
  begin
    for iCount := 0 to RegP001.RegistroP200.Count-1 do
    begin
      with RegP001.RegistroP200.Items[iCount] do
      begin
        Add(
           LFill( 'P200' )              +
           LFill( PER_REF)              +
           LFill( VL_TOT_CONT_APU,0, 2 )+
           LFill( VL_TOT_AJ_REDUC,0, 2 )+
           LFill( VL_TOT_AJ_ACRES,0, 2 )+
           LFill( VL_TOT_CONT_DEV,0, 2 )+
           LFill( COD_REC)
         );
      end;
      // Registros FILHOS
      WriteRegistroP210( RegP001.RegistroP200.Items[iCount] );

      RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
    end;
    fRegistroP200Count := FRegistroP200Count + RegP001.RegistroP200.Count;
  end;
end;

(*By: Edilson Alves de Oliveira*)
procedure TBloco_P.WriteRegistroP110(RegP100: TRegistroP100);
var
  iCount: Integer;
begin
  if Assigned(RegP100.RegistroP110 ) then
  begin
     for iCount := 0 to RegP100.RegistroP110.Count - 1 do
     begin
       with RegP100.RegistroP110.Items[iCount] do
       begin
         Add(LFill( 'P110')        +
             LFill( NUM_CAMPO)     +
             LFill( COD_DET)       +
             LFill( DET_VALOR,0, 2)+
             LFill(INF_COMPL)
             );
       end;
       RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP110Count := FRegistroP110Count + RegP100.RegistroP110.Count;
  end;
end;

(*By Edilson Alves de Oliveira*)
procedure TBloco_P.WriteRegistroP199(RegP100: TRegistroP100);
var 
  iCount: Integer;
begin
  if Assigned( RegP100.RegistroP199 ) then
  begin
     for iCount := 0 to RegP100.RegistroP199.Count - 1 do
     begin
       with RegP100.RegistroP199.Items[iCount] do
       begin
         Add(LFill( 'P199')   +
             LFill( NUM_PROC) +
             LFill(IND_PROC)
             );
       end;
       RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP199Count := FRegistroP199Count + RegP100.RegistroP199.Count;
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

function TBloco_P.RegistroP110New: TRegistroP110;
var
  P010Count: integer;
  P100Count: integer;
begin
   P010Count := FRegistroP001.RegistroP010.Count -1;
   P100Count := FRegistroP001.RegistroP010.Items[P010Count].RegistroP100.Count -1;
   Result    := FRegistroP001.RegistroP010.Items[P010Count].RegistroP100.Items[P100Count].RegistroP110.New;
end;

function TBloco_P.RegistroP210New: TRegistroP210;
var
  P200Count: integer;
begin
   P200Count := FRegistroP001.RegistroP200.Count -1;
   //
   Result    := FRegistroP001.RegistroP200.Items[P200Count].RegistroP210.New;
end;

function TBloco_P.RegistroP199New: TRegistroP199;
var
  P010Count : integer;
  P100Count : integer;
begin
   P010Count := FRegistroP001.RegistroP010.Count -1;
   P100Count := FRegistroP001.RegistroP010.Items[P010Count].RegistroP100.Count -1;
   Result    := FRegistroP001.RegistroP010.Items[P010Count].RegistroP100.Items[P100Count].RegistroP199.New;
end;

function TBloco_P.RegistroP200New: TRegistroP200;
begin
   Result := FRegistroP001.RegistroP200.New;
end;

procedure TBloco_P.WriteRegistroP210(RegP200: TRegistroP200);
var
  iCount:Integer;
begin
  if Assigned( RegP200.RegistroP210 ) then
  begin
     for iCount := 0 to RegP200.RegistroP210.Count - 1 do
     begin
       with RegP200.RegistroP210.Items[iCount] do
       begin
         Add(LFill( 'P210')             +
             LFill( IND_AJ)             +
             LFill( VL_AJ,0, 2)         +
             LFill( COD_AJ)             +
             LFill( NUM_DOC)            +
             LFill( DESCR_AJ)           +
             LFill( DT_REF, 'ddmmyyyy')
             );
       end;
       RegistroP990.QTD_LIN_P := RegistroP990.QTD_LIN_P + 1;
     end;
     FRegistroP210Count := FRegistroP210Count + RegP200.RegistroP210.Count;
  end;
end;

end.
