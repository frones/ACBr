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
|* 14/02/2014: Juliomar Marchetti
|*  - Criação bloco K - alterado
*******************************************************************************}

unit ACBrEFDBloco_K_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_K, ACBrEFDBlocos,
     ACBrEFDBloco_0_Class;

type
  /// TBLOCO_K -

  { TBloco_K }

  TBloco_K = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroK001: TRegistroK001;      /// BLOCO K - RegistroK001
    FRegistroK990: TRegistroK990;      /// BLOCO K - RegistroK990

    FRegistroK100Count: Integer;
    FRegistroK200Count: Integer;
    FRegistroK220Count: Integer;
    FRegistroK230Count: Integer;
    FRegistroK235Count: Integer;
    FRegistroK250Count: Integer;
    FRegistroK255Count: Integer;

    procedure WriteRegistroK100(RegK001: TRegistroK001);
    procedure WriteRegistroK200(RegK100: TRegistroK100);
    procedure WriteRegistroK220(RegK100: TRegistroK100);
    procedure WriteRegistroK230(RegK100: TRegistroK100);
    procedure WriteRegistroK235(RegK230: TRegistroK230);
    procedure WriteRegistroK250(RegK100: TRegistroK100);
    procedure WriteRegistroK255(RegK250: TRegistroK250);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function RegistroK001New: TRegistroK001;
    function RegistroK100New: TRegistroK100;
    function RegistroK200New: TRegistroK200;
    function RegistroK220New: TRegistroK220;
    function RegistroK230New: TRegistroK230;
    function RegistroK235New: TRegistroK235;
    function RegistroK250New: TRegistroK250;
    function RegistroK255New: TRegistroK255;

    procedure WriteRegistroK001;
    procedure WriteRegistroK990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroK001: TRegistroK001 read FRegistroK001 write FRegistroK001;
    property RegistroK990: TRegistroK990 read FRegistroK990 write FRegistroK990;

    property RegistroK100Count: Integer read FRegistroK100Count write FRegistroK100Count;
    property RegistroK200Count: Integer read FRegistroK200Count write FRegistroK200Count;
    property RegistroK220Count: Integer read FRegistroK220Count write FRegistroK220Count;
    property RegistroK230Count: Integer read FRegistroK230Count write FRegistroK230Count;
    property RegistroK235Count: Integer read FRegistroK235Count write FRegistroK235Count;
    property RegistroK250Count: Integer read FRegistroK250Count write FRegistroK250Count;
    property RegistroK255Count: Integer read FRegistroK255Count write FRegistroK255Count;

  end;

implementation

{ TBloco_K }

procedure TBloco_K.WriteRegistroK100(RegK001: TRegistroK001);
var
  intFor: integer;
begin
  if Assigned(RegK001.RegistroK100) then
  begin
     for intFor := 0 to RegK001.RegistroK100.Count - 1 do
     begin
        with RegK001.RegistroK100.Items[intFor] do
        begin
          if (DT_INI < Bloco_0.DT_INI)or (DT_INI > Bloco_0.DT_FIN) then
             raise Exception.Create('A data inicial está fora do periodo do EFD!');
          if (DT_FIN < Bloco_0.DT_INI)or (DT_FIN > Bloco_0.DT_FIN) then
             raise Exception.Create('A data final está fora do periodo do EFD!');

          Add( LFill('K100') +
               LFill( DT_INI) +
               LFill( DT_FIN) );
          WriteRegistroK200( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK220( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK230( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK250( RegK001.RegistroK100.Items[intFor] );
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK100Count := FRegistroK100Count + RegK001.RegistroK100.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK200(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK200 ) then
  begin
     for intFor := 0 to RegK100.RegistroK200.Count - 1 do
     begin
        with RegK100.RegistroK200.Items[intFor] do
        begin
          if DT_EST <> DT_FIN then
             raise Exception.Create('A data do estoque deve ser igual à data final do período de apuração – campo DT_FIN do Registro K100');
          if IND_EST in [estPropInformanteTerceiros,estPropTerceirosInformante] then
             if Trim(COD_PART) = EmptyStr then
                raise Exception.Create('O campo COD_PART será obrigatório conforme informação do campo IND_EST');

          Add( LFill('K200') +
               LFill( DT_EST ) +
               LFill( COD_ITEM  ) +
               DFill( QTD  , 3 ) +
               LFill( Integer(IND_EST), 0 ) +
               LFill( COD_PART ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK200Count := FRegistroK200Count + RegK100.RegistroK200.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK220(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK220 ) then
  begin
     for intFor := 0 to RegK100.RegistroK220.Count - 1 do
     begin
        with RegK100.RegistroK220.Items[intFor] do
        begin
          if (DT_MOV < DT_INI) or (DT_MOV > DT_FIN) then
             raise Exception.Create('A data deve estar compreendida no período informado nos campos DT_INI e DT_FIN do Registro K100');

          Add( LFill('K220') +
               LFill( DT_MOV ) +
               LFill( COD_ITEM_ORI  ) +
               LFill( COD_ITEM_DEST  ) +
               LFill( QTD , 0, 3 ) );
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK220Count := FRegistroK220Count + RegK100.RegistroK220.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK230(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK230 ) then
  begin
     for intFor := 0 to RegK100.RegistroK230.Count - 1 do
     begin
        with RegK100.RegistroK230.Items[intFor] do
        begin
          if DT_INI_OP > Bloco_0.DT_FIN then
             raise Exception.Create('O valor informado deve ser menor ou igual a DT_FIN do registro 0000');
          if DT_FIN_OP > 0 then
             if (DT_FIN_OP > DT_FIN)or ( DT_FIN_OP < DT_INI_OP) then
                raise Exception.Create('Se preenchido, DT_FIN_OP deve ser menor ou igual a DT_FIN do registro K100 e maior ou igual a DT_INI_OP');

          Add( LFill('K230') +
               LFill( DT_INI_OP ) +
               LFill( DT_FIN_OP ) +
               LFill( COD_DOC_OP  ) +
               LFill( COD_ITEM  ) +
               DFill( QTD_ENC, 3 ));

          WriteRegistroK235(RegK100.RegistroK230.Items[intFor]);
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK230Count := FRegistroK230Count + RegK100.RegistroK230.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK235(RegK230: TRegistroK230);
var
  intFor: integer;
begin
  if Assigned( RegK230.RegistroK235 ) then
  begin
     for intFor := 0 to RegK230.RegistroK235.Count - 1 do
     begin
        with RegK230.RegistroK235.Items[intFor] do
        begin
          Add( LFill('K235') +
               LFill( DT_SAIDA ) +
               LFill( COD_ITEM  ) +
               DFill( QTD , 3 ) +
               LFill( COD_INS_SUBST  ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK235Count := FRegistroK235Count + RegK230.RegistroK235.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK250(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK250 ) then
  begin
     for intFor := 0 to RegK100.RegistroK250.Count - 1 do
     begin
        with RegK100.RegistroK250.Items[intFor] do
        begin
          Add( LFill('K250') +
               LFill( DT_PROD ) +
               LFill( COD_ITEM  ) +
               LFill( QTD , 0, 3 ));
		  WriteRegistroK255(RegK100.RegistroK250.Items[intFor]);
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK250Count := FRegistroK250Count + RegK100.RegistroK250.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK255(RegK250: TRegistroK250);
var
  intFor: integer;
begin
  if Assigned( RegK250.RegistroK255 ) then
  begin
     for intFor := 0 to RegK250.RegistroK255.Count - 1 do
     begin
        with RegK250.RegistroK255.Items[intFor] do
        begin
          Add( LFill('K255') +
               LFill( DT_CONS ) +
               LFill( COD_ITEM  ) +
               LFill( QTD , 0, 3 )+
               LFill( COD_INS_SUBST));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK255Count := FRegistroK255Count + RegK250.RegistroK255.Count;
  end;
end;

procedure TBloco_K.CriaRegistros;
begin
  FRegistroK001 := TRegistroK001.Create;
  FRegistroK990 := TRegistroK990.Create;

  FRegistroK100Count := 0;
  FRegistroK200Count := 0;
  FRegistroK220Count := 0;
  FRegistroK230Count := 0;
  FRegistroK235Count := 0;
  FRegistroK250Count := 0;
  FRegistroK255Count := 0;

  FRegistroK990.QTD_LIN_K := 0;
end;

procedure TBloco_K.LiberaRegistros;
begin
  FRegistroK001.Free;
  FRegistroK990.Free;
end;

constructor TBloco_K.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_K.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_K.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_K.RegistroK001New: TRegistroK001;
begin
  Result := FRegistroK001;
end;

function TBloco_K.RegistroK100New: TRegistroK100;
begin
  Result := FRegistroK001.RegistroK100.New(FRegistroK001);
end;

function TBloco_K.RegistroK200New: TRegistroK200;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K200 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK200.New(K100);
end;

function TBloco_K.RegistroK220New: TRegistroK220;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K220 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK220.New(K100);
end;

function TBloco_K.RegistroK230New: TRegistroK230;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K230 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK230.New(K100);
end;

function TBloco_K.RegistroK235New: TRegistroK235;
var
   K230: TRegistroK230;
   K100Count: integer;
   K230Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K230Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK230.Count -1;
   if K230Count = -1 then
      raise Exception.Create('O registro K235 deve ser filho do registro K230, e não existe nenhum K230 pai!');

   K230   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK230.Items[K230Count];
   Result := K230.RegistroK235.New(K230);
end;

function TBloco_K.RegistroK250New: TRegistroK250;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K250 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK250.New(K100);
end;

function TBloco_K.RegistroK255New: TRegistroK255;
var
   K250: TRegistroK250;
   K100Count: integer;
   K250Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K250Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK250.Count -1;
   if K250Count = -1 then
      raise Exception.Create('O registro K255 deve ser filho do registro K250, e não existe nenhum K250 pai!');

   K250   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK250.Items[K250Count];
   Result := K250.RegistroK255.New(K250);
end;

procedure TBloco_K.WriteRegistroK001;
begin
  if Assigned(RegistroK001) then
  begin
     with RegistroK001 do
     begin
       Add( LFill( 'K001' ) +
            LFill( Integer(IND_MOV), 0 ) ) ;

       if IND_MOV = imComDados then
       begin
          WriteRegistroK100(FRegistroK001);
       end;
     end;

     RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
  end;
end;

procedure TBloco_K.WriteRegistroK990;
begin
  if Assigned(RegistroK990) then
  begin
     with RegistroK990 do
     begin
       QTD_LIN_K := QTD_LIN_K + 1;
       ///
       Add( LFill('K990') +
            LFill(QTD_LIN_K,0) ) ;
     end;
  end;
end;

end.

