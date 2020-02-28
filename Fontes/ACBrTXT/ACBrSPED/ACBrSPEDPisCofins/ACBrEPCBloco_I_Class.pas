{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Juliomar Marchetti            }
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

unit ACBrEPCBloco_I_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEPCBloco_I, ACBrEPCBlocos,
     ACBrEPCBloco_0_Class;

type
  /// TBloco_I - Abertura, Identificação e Referências

  { TBloco_I }

  TBloco_I = class(TACBrSPED)
  private
    FRegistroI001: TRegistroI001;      /// BLOCO I - RegistroI001
    FRegistroI990: TRegistroI990;      /// BLOCO I - RegistroI990

    FRegistroI010Count: Integer;
    FRegistroI100Count: Integer;
    FRegistroI199Count: Integer;
    FRegistroI200Count: Integer;
    FRegistroI299Count: Integer;
    FRegistroI300Count: Integer;
    FRegistroI399Count: Integer;
    FBloco_0: TBloco_0;

    procedure WriteRegistroI010(RegI001: TRegistroI001);
    procedure WriteRegistroI100(RegI010: TRegistroI010);
    procedure WriteRegistroI199(RegI100: TRegistroI100);
    procedure WriteRegistroI200(RegI100: TRegistroI100);
    procedure WriteRegistroI299(RegI200: TRegistroI200);
    procedure WriteRegistroI300(RegI200: TRegistroI200);
    procedure WriteRegistroI399(RegI300: TRegistroI300);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create ;          /// Create
    destructor  Destroy; override; /// Destroy

    procedure LimpaRegistros; override;

    function RegistroI001New: TRegistroI001;
    function RegistroI010New: TRegistroI010;
    function RegistroI100New: TRegistroI100;
    function RegistroI199New: TRegistroI199;
    function RegistroI200New: TRegistroI200;
    function RegistroI299New: TRegistroI299;
    function RegistroI300New: TRegistroI300;
    function RegistroI399New: TRegistroI399;

    procedure WriteRegistroI001 ;
    procedure WriteRegistroI990 ;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroI001: TRegistroI001 read FRegistroI001 write FRegistroI001;
    property RegistroI990: TRegistroI990 read FRegistroI990 write FRegistroI990;

    property RegistroI010Count: Integer read FRegistroI010Count write FRegistroI010Count;
    property RegistroI100Count: Integer read FRegistroI100Count write FRegistroI100Count;
    property RegistroI199Count: Integer read FRegistroI199Count write FRegistroI199Count;
    property RegistroI200Count: Integer read FRegistroI200Count write FRegistroI200Count;
    property RegistroI299Count: Integer read FRegistroI299Count write FRegistroI299Count;
    property RegistroI300Count: Integer read FRegistroI300Count write FRegistroI300Count;
    property RegistroI399Count: Integer read FRegistroI399Count write FRegistroI399Count;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_I }

procedure TBloco_I.WriteRegistroI010(RegI001: TRegistroI001);
var
   intFor : Integer;
begin
  if Assigned(RegI001.RegistroI010) then
  begin
    if (FRegistroI010Count < RegI001.RegistroI010.Count) then // Algum I010 ainda nao gravado?
    begin
      for intFor := FRegistroI010Count to RegI001.RegistroI010.Count - 1 do
      begin
        with RegI001.RegistroI010.Items[intFor] do
        begin
          Check(funChecaCNPJ(CNPJ), '(I-010) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);

          Add( LFill('I010') +
               LFill(CNPJ, 14) +
               LFill(IND_ATIV,2)+
               LFill(INFO_COMPL)) ;
        end;

        // Registros FILHOS
        WriteRegistroI100( RegI001.RegistroI010.Items[intFor] );
        //
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
      end;
      // Variavél para armazenar a quantidade de registro do tipo.
      FRegistroI010Count := FRegistroI010Count + RegI001.RegistroI010.Count;
    end
    else // apenas gravar os registros FILHOS do ultimo I010 existente
    begin
      // Registros FILHOS
      WriteRegistroI100( RegI001.RegistroI010.Items[FRegistroI010Count-1] );
    end;
  end;
end;

procedure TBloco_I.WriteRegistroI100(RegI010: TRegistroI010);
var
  intFor      : integer;
begin
  if Assigned(RegI010.RegistroI100) then
  begin
     for intFor := 0 to RegI010.RegistroI100.Count - 1 do
     begin
        with RegI010.RegistroI100.Items[intFor] do
        begin
          Add( LFill('I100')                             +
               LFill( VL_REC, 0 , 2 )                    +
               LFill( CstPisCofinsToStr(CST_PIS_COFINS) )+
               LFill( VL_TOT_DED_GER, 0, 2 )             +
               LFill( VL_TOT_DED_ESP, 0, 2 )             +
               LFill( VL_BC_PIS, 0, 2 )                  +
               LFill( ALIQ_PIS, 8, 2 )                   +
               LFill( VL_PIS, 0, 2 )                     +
               LFill( VL_BC_COFINS, 0, 2 )               +
               LFill( ALIQ_COFINS, 8, 2 )                +
               LFill( VL_COFINS, 0, 2 )                  +
               LFill( INFO_COMPL )) ;
        end;
        // Registros FILHOS
        WriteRegistroI199( RegI010.RegistroI100.Items[intFor] );
        WriteRegistroI200( RegI010.RegistroI100.Items[intFor] );
        ///
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroI100Count := FRegistroI100Count + RegI010.RegistroI100.Count;
     //
     RegI010.RegistroI100.Clear;
  end;
end;

procedure TBloco_I.WriteRegistroI199(RegI100: TRegistroI100);
var
   intFor : integer;
   strIND_PROC: string;
begin
  if Assigned(RegI100.RegistroI199) then
  begin
    for intFor := 0 to RegI100.RegistroI199.Count - 1 do
    begin
      with RegI100.RegistroI199.Items[intFor] do
      begin
        if IND_PROC = opJusticaFederal then
           strIND_PROC := '1'
        else if IND_PROC = opSecexRFB then
           strIND_PROC := '3'
        else if IND_PROC = opOutros then
           strIND_PROC := '9'
        else
          strIND_PROC := ' ';

        Add( LFill('I199')  +
             LFill(NUM_PROC) +
             LFill(strIND_PROC, 1)) ;
        //
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
      end;
   end;
   // Variavél para armazenar a quantidade de registro do tipo.
   FRegistroI199Count := FRegistroI199Count + RegI100.RegistroI199.Count;
  end;
end;

procedure TBloco_I.WriteRegistroI200(RegI100: TRegistroI100);
var
    intFor : integer;
begin
  if Assigned(RegI100.RegistroI200) then
  begin
    for intFor := 0 to RegI100.RegistroI200.Count - 1 do
    begin
      with RegI100.RegistroI200.Items[intFor] do
      begin
        Add( LFill('I200')  +
             LFill(NUM_CAMPO, 2) +
             LFill(COD_DET, 5)   +
             LFill(DET_VALOR, 0, 2)+
             LFill(COD_CTA) +
             LFill(INFO_COMPL)) ;

        WriteRegistroI299(RegI100.RegistroI200.Items[intFor]);
        WriteRegistroI300(RegI100.RegistroI200.Items[intFor]);
        //
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
      end;
   end;
   // Variavél para armazenar a quantidade de registro do tipo.
   FRegistroI200Count := FRegistroI200Count + RegI100.RegistroI200.Count;
  end;
end;

procedure TBloco_I.WriteRegistroI299(RegI200: TRegistroI200);
var
   intFor : integer;
   strIND_PROC: string;
begin
  if Assigned(RegI200.RegistroI299) then
  begin
    for intFor := 0 to RegI200.RegistroI299.Count - 1 do
    begin
      with RegI200.RegistroI299.Items[intFor] do
      begin
        if IND_PROC = opJusticaFederal then
           strIND_PROC := '1'
        else if IND_PROC = opSecexRFB then
           strIND_PROC := '3'
        else if IND_PROC = opOutros then
           strIND_PROC := '9'
        else
          strIND_PROC := ' ';

        Add( LFill('I299')  +
             LFill(NUM_PROC) +
             LFill(strIND_PROC, 1)) ;
        //
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
      end;
   end;
   // Variavél para armazenar a quantidade de registro do tipo.
   FRegistroI299Count := FRegistroI299Count + RegI200.RegistroI299.Count;
  end;
end;

procedure TBloco_I.WriteRegistroI300(RegI200: TRegistroI200);
var
  intFor      : integer;
begin
  if Assigned(RegI200.RegistroI300) then
  begin
     for intFor := 0 to RegI200.RegistroI300.Count - 1 do
     begin
        with RegI200.RegistroI300.Items[intFor] do
        begin
          Add( LFill('I300')                             +
               LFill( COD_COMP )                    +
               LFill( DET_VALOR, 0 , 2 )+
               LFill( COD_CTA )             +
               LFill( INFO_COMPL )) ;
        end;
        WriteRegistroI399(RegI200.RegistroI300.Items[intFor]);
        ///
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroI300Count := FRegistroI300Count + RegI200.RegistroI300.Count;
     //
     RegI200.RegistroI300.Clear;
  end;
end;

procedure TBloco_I.WriteRegistroI399(RegI300: TRegistroI300);
var
  intFor      : integer;
  strIND_PROC : string;
begin
  if Assigned(RegI300.RegistroI399) then
  begin
     for intFor := 0 to RegI300.RegistroI399.Count - 1 do
     begin
        with RegI300.RegistroI399.Items[intFor] do
        begin
          if IND_PROC = opJusticaFederal then
             strIND_PROC := '1'
          else if IND_PROC = opSecexRFB then
             strIND_PROC := '3'
          else if IND_PROC = opOutros then
             strIND_PROC := '9'
          else
            strIND_PROC := ' ';
          Add( LFill('I399')                             +
               LFill( NUM_PROC )                    +
               LFill( strIND_PROC, 1 )) ;
        end;
        ///
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroI399Count := FRegistroI399Count + RegI300.RegistroI399.Count;
     //
     RegI300.RegistroI399.Clear;
  end;
end;

procedure TBloco_I.CriaRegistros;
begin
  FRegistroI001           := TRegistroI001.Create;
  FRegistroI990           := TRegistroI990.Create;

  FRegistroI010Count      := 0;
  FRegistroI100Count      := 0;
  FRegistroI200Count      := 0;
  FRegistroI299Count      := 0;
  FRegistroI300Count      := 0;
  FRegistroI399Count      := 0;

  FRegistroI990.QTD_LIN_I := 0;
end;

procedure TBloco_I.LiberaRegistros;
begin
  FRegistroI001.Free;
  FRegistroI990.Free;
end;

constructor TBloco_I.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_I.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_I.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_I.RegistroI001New: TRegistroI001;
begin
  Result := FRegistroI001;
end;

function TBloco_I.RegistroI010New: TRegistroI010;
begin
  Result := FRegistroI001.RegistroI010.New;
end;

function TBloco_I.RegistroI100New: TRegistroI100;
var
   I010Count : integer;
begin
   I010Count := FRegistroI001.RegistroI010.Count -1;
   //
   Result    := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.New;
end;

function TBloco_I.RegistroI199New: TRegistroI199;
var
   I010Count : integer;
   I100Count : integer;
begin
   I010Count := FRegistroI001.RegistroI010.Count -1;
   I100Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Count -1;
   //
   Result    := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI199.New;
end;

function TBloco_I.RegistroI200New: TRegistroI200;
var
   I010Count : integer;
   I100Count : integer;
begin
   I010Count := FRegistroI001.RegistroI010.Count -1;
   I100Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Count -1;
   //
   Result    := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.New;
end;

function TBloco_I.RegistroI299New: TRegistroI299;
var
   I010Count : integer;
   I100Count : integer;
   I200Count : integer;
begin
   I010Count := FRegistroI001.RegistroI010.Count -1;
   I100Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Count -1;
   I200Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.Count -1;
   //
   Result    := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.Items[I200Count].RegistroI299.New;
end;

function TBloco_I.RegistroI300New: TRegistroI300;
var
   I010Count : integer;
   I100Count : integer;
   I200Count : integer;
begin
   I010Count := FRegistroI001.RegistroI010.Count -1;
   I100Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Count -1;
   I200Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.Count -1;
   //
   Result    := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.Items[I200Count].RegistroI300.New;
end;

function TBloco_I.RegistroI399New: TRegistroI399;
var
   I010Count : integer;
   I100Count : integer;
   I200Count : integer;
   I300Count : integer;
begin
   I010Count := FRegistroI001.RegistroI010.Count -1;
   I100Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Count -1;
   I200Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.Count -1;
   I300Count := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.Items[I200Count].RegistroI300.Count -1;
   //
   Result    := FRegistroI001.RegistroI010.Items[I010Count].RegistroI100.Items[I100Count].RegistroI200.Items[I200Count].RegistroI300.Items[I300Count].RegistroI399.New;
end;

procedure TBloco_I.WriteRegistroI001;
begin
  if Assigned(FRegistroI001) then
  begin
     if (RegistroI990.QTD_LIN_I = 0) then  // Ja gravou o I001?
     begin
        with FRegistroI001 do
        begin
           Add( LFill( 'I001' ) +
                LFill( Integer(IND_MOV), 0 ) ) ;
        end;
        RegistroI990.QTD_LIN_I := RegistroI990.QTD_LIN_I + 1;
     end;
     if FRegistroI001.IND_MOV = imComDados then
     begin
       WriteRegistroI010(FRegistroI001) ;
     end;
  end;
end;

procedure TBloco_I.WriteRegistroI990;
begin
  if Assigned(RegistroI990) then
  begin
     with RegistroI990 do
     begin
       QTD_LIN_I := QTD_LIN_I + 1;
       ///
       Add( LFill('I990') +
            LFill(QTD_LIN_I,0) );
     end;
  end;
end;

end.

