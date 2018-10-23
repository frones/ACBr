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
|*
|* 26/10/2017 - (Edilson) - Validação K270, K275 e K280:
|*  - Não enviar valor negativo, enviar somente um dos valores positivo ou
|*    negativo.
*******************************************************************************}

unit ACBrEFDBloco_K_Class;

interface

uses SysUtils, StrUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_K, ACBrEFDBlocos,
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
    FRegistroK210Count: Integer;
    FRegistroK215Count: Integer;
    FRegistroK220Count: Integer;
    FRegistroK230Count: Integer;
    FRegistroK235Count: Integer;
    FRegistroK250Count: Integer;
    FRegistroK255Count: Integer;
    FRegistroK260Count: Integer;
    FRegistroK265Count: Integer;
    FRegistroK270Count: Integer;
    FRegistroK275Count: Integer;
    FRegistroK280Count: Integer;
    FRegistroK290Count: Integer;
    FRegistroK291Count: Integer;
    FRegistroK292Count: Integer;
    FRegistroK300Count: Integer;
    FRegistroK301Count: Integer;
    FRegistroK302Count: Integer;

    procedure WriteRegistroK100(RegK001: TRegistroK001);
    procedure WriteRegistroK200(RegK100: TRegistroK100);
    procedure WriteRegistroK210(RegK100: TRegistroK100);
    procedure WriteRegistroK215(RegK210: TRegistroK210);
    procedure WriteRegistroK220(RegK100: TRegistroK100);
    procedure WriteRegistroK230(RegK100: TRegistroK100);
    procedure WriteRegistroK235(RegK230: TRegistroK230);
    procedure WriteRegistroK250(RegK100: TRegistroK100);
    procedure WriteRegistroK255(RegK250: TRegistroK250);
    procedure WriteRegistroK260(RegK100: TRegistroK100);
    procedure WriteRegistroK265(RegK260: TRegistroK260);
    procedure WriteRegistroK270(RegK100: TRegistroK100);
    procedure WriteRegistroK275(RegK270: TRegistroK270);
    procedure WriteRegistroK280(RegK100: TRegistroK100);
    procedure WriteRegistroK290(RegK100: TRegistroK100);
    procedure WriteRegistroK291(RegK290: TRegistroK290);
    procedure WriteRegistroK292(RegK290: TRegistroK290);
    procedure WriteRegistroK300(RegK100: TRegistroK100);
    procedure WriteRegistroK301(RegK300: TRegistroK300);
    procedure WriteRegistroK302(RegK300: TRegistroK300);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function RegistroK001New: TRegistroK001;
    function RegistroK100New: TRegistroK100;
    function RegistroK200New: TRegistroK200;
    function RegistroK210New: TRegistroK210;
    function RegistroK215New: TRegistroK215;
    function RegistroK220New: TRegistroK220;
    function RegistroK230New: TRegistroK230;
    function RegistroK235New: TRegistroK235;
    function RegistroK250New: TRegistroK250;
    function RegistroK255New: TRegistroK255;
    function RegistroK260New: TRegistroK260;
    function RegistroK265New: TRegistroK265;
    function RegistroK270New: TRegistroK270;
    function RegistroK275New: TRegistroK275;
    function RegistroK280New: TRegistroK280;
    function RegistroK290New: TRegistroK290;
    function RegistroK291New: TRegistroK291;
    function RegistroK292New: TRegistroK292;
    function RegistroK300New: TRegistroK300;
    function RegistroK301New: TRegistroK301;
    function RegistroK302New: TRegistroK302;

    procedure WriteRegistroK001;
    procedure WriteRegistroK990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroK001: TRegistroK001 read FRegistroK001 write FRegistroK001;
    property RegistroK990: TRegistroK990 read FRegistroK990 write FRegistroK990;

    property RegistroK100Count: Integer read FRegistroK100Count write FRegistroK100Count;
    property RegistroK200Count: Integer read FRegistroK200Count write FRegistroK200Count;
    property RegistroK210Count: Integer read FRegistroK210Count write FRegistroK210Count;
    property RegistroK215Count: Integer read FRegistroK215Count write FRegistroK215Count;
    property RegistroK220Count: Integer read FRegistroK220Count write FRegistroK220Count;
    property RegistroK230Count: Integer read FRegistroK230Count write FRegistroK230Count;
    property RegistroK235Count: Integer read FRegistroK235Count write FRegistroK235Count;
    property RegistroK250Count: Integer read FRegistroK250Count write FRegistroK250Count;
    property RegistroK255Count: Integer read FRegistroK255Count write FRegistroK255Count;
    property RegistroK260Count: Integer read FRegistroK260Count write FRegistroK260Count;
    property RegistroK265Count: Integer read FRegistroK265Count write FRegistroK265Count;
    property RegistroK270Count: Integer read FRegistroK270Count write FRegistroK270Count;
    property RegistroK275Count: Integer read FRegistroK275Count write FRegistroK275Count;
    property RegistroK280Count: Integer read FRegistroK280Count write FRegistroK280Count;
    property RegistroK290Count: Integer read FRegistroK290Count write FRegistroK290Count;
    property RegistroK291Count: Integer read FRegistroK291Count write FRegistroK291Count;
    property RegistroK292Count: Integer read FRegistroK292Count write FRegistroK292Count;
    property RegistroK300Count: Integer read FRegistroK300Count write FRegistroK300Count;
    property RegistroK301Count: Integer read FRegistroK301Count write FRegistroK301Count;
    property RegistroK302Count: Integer read FRegistroK302Count write FRegistroK302Count;
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
          WriteRegistroK210( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK220( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK230( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK250( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK260( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK270( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK280( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK290( RegK001.RegistroK100.Items[intFor] );
          WriteRegistroK300( RegK001.RegistroK100.Items[intFor] );
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
          if DT_EST <> RegK100.DT_FIN then
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

procedure TBloco_K.WriteRegistroK210(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK210 ) then
  begin
     for intFor := 0 to RegK100.RegistroK210.Count - 1 do
     begin
        with RegK100.RegistroK210.Items[intFor] do
        begin
          if ((COD_DOC_OS <> '') or (DT_FIN_OS > 0)) and (DT_INI_OS <= 0) then
            raise Exception.Create('O campo DT_INI_OS será obrigatório conforme informação do campo COD_DOC_OS ou DT_FIN_OS');
          if (DT_INI_OS > RegK100.DT_FIN) or (DT_FIN_OS > RegK100.DT_INI) then
           raise Exception.Create('A data deve estar compreendida no período informado nos campos DT_INI e DT_FIN do Registro K100');
          if DT_INI_OS > DT_FIN_OS then
            raise Exception.Create('O campo DT_INI_OS não pode ser maior do que o campo DT_FIN_OS');
          if ((DT_INI_OS > 0) or (DT_FIN_OS > 0)) and (COD_DOC_OS = '') then
            raise Exception.Create('O campo COD_DOC_OS será obrigatório conforme informação do campo DT_INI_OS ou DT_FIN_OS');

          Add( LFill('K210') +
               LFill( DT_INI_OS ) +
               LFill( DT_FIN_OS ) +
               LFill( COD_DOC_OS  ) +
               LFill( COD_ITEM_ORI  ) +
               DFill( QTD_ORI, 3 ) );

          WriteRegistroK215(RegK100.RegistroK210.Items[intFor]);
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK210Count := FRegistroK210Count + RegK100.RegistroK210.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK215(RegK210: TRegistroK210);
var
  intFor: integer;
begin
  if Assigned( RegK210.RegistroK215 ) then
  begin
     for intFor := 0 to RegK210.RegistroK215.Count - 1 do
     begin
        with RegK210.RegistroK215.Items[intFor] do
        begin
          Add( LFill('K215') +
               LFill( COD_ITEM_DES ) +
               DFill( QTD_DES, 3 ) );
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK215Count := FRegistroK215Count + RegK210.RegistroK215.Count;
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
          if (DT_MOV < RegK100.DT_INI) or (DT_MOV > RegK100.DT_FIN) then
             raise Exception.Create('A data deve estar compreendida no período informado nos campos DT_INI e DT_FIN do Registro K100');

          Add( LFill('K220') +
               LFill( DT_MOV ) +
               LFill( COD_ITEM_ORI  ) +
               LFill( COD_ITEM_DEST  ) +
               DFill( QTD, 3 )+
               IfThen(DT_INI >= EncodeDate(2018,01,01),
			           DFill( QTD_DEST, 3 ), EmptyStr));
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
          if ((COD_DOC_OP <> '') or (DT_FIN_OP > 0)) and (DT_INI_OP <= 0) then
            raise Exception.Create('O campo DT_INI_OS será obrigatório conforme informação do campo COD_DOC_OP ou DT_FIN_OP');
          if ((DT_INI_OP > 0) or (DT_FIN_OP > 0)) and (COD_DOC_OP = '') then
            raise Exception.Create('O campo COD_DOC_OP será obrigatório conforme informação do campo DT_INI_OS ou DT_FIN_OS');

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
          if (DT_SAIDA < RegK230.DT_INI_OP) then
           raise Exception.Create('A data de saída deve ser igual ou posterior ao início da produção, informado em DT_INI_OP do Registro K230');

          Add( LFill('K235') +
               LFill( RegK230.RegistroK235.Items[intFor].DT_SAIDA ) +
               LFill( COD_ITEM ) +
               DFill( QTD , 3 ) +
               LFill( COD_INS_SUBST ));
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
          if (DT_PROD < RegK100.DT_INI) or (DT_PROD > RegK100.DT_FIN) then
           raise Exception.Create('A data deve estar compreendida no período informado nos campos DT_INI e DT_FIN do Registro K100');

          Add( LFill('K250') +
               LFill( DT_PROD ) +
               LFill( COD_ITEM  ) +
               DFill( QTD, 3 ) );
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
               DFill( QTD, 3 ) +
               LFill( COD_INS_SUBST));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK255Count := FRegistroK255Count + RegK250.RegistroK255.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK260(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK260 ) then
  begin
     for intFor := 0 to RegK100.RegistroK260.Count - 1 do
     begin
        with RegK100.RegistroK260.Items[intFor] do
        begin
          if (DT_SAIDA < RegK100.DT_INI) or (DT_SAIDA > RegK100.DT_FIN) then
             raise Exception.Create('A data de saída deve estar compreendida no período informado nos campos DT_INI e DT_FIN do Registro K100');

          Add( LFill('K260') +
               LFill( COD_OP_OS ) +
               LFill( COD_ITEM ) +
               LFill( DT_SAIDA ) +
               DFill( QTD_SAIDA , 3 ) +
               LFill( DT_RET ) +
               DFill( QTD_RET , 3 ));

          WriteRegistroK265(RegK100.RegistroK260.Items[intFor]);
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK260Count := FRegistroK260Count + RegK100.RegistroK260.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK265(RegK260: TRegistroK260);
var
  intFor: integer;
begin
  if Assigned( RegK260.RegistroK265 ) then
  begin
     for intFor := 0 to RegK260.RegistroK265.Count - 1 do
     begin
        with RegK260.RegistroK265.Items[intFor] do
        begin
          Add( LFill('K265') +
               LFill( COD_ITEM ) +
               DFill( QTD_CONS , 3 ) +
               DFill( QTD_RET , 3 ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK265Count := FRegistroK265Count + RegK260.RegistroK265.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK270(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK270 ) then
  begin
     for intFor := 0 to RegK100.RegistroK270.Count - 1 do
     begin
        with RegK100.RegistroK270.Items[intFor] do
        begin
          Add( LFill('K270') +
               LFill( DT_INI_AP ) +
               LFill( DT_FIN_AP ) +
               LFill( COD_OP_OS ) +
               LFill( COD_ITEM ) +
               DFill( QTD_COR_POS , 3 ,(QTD_COR_POS<=0)) +
               DFill( QTD_COR_NEG , 3 ,(QTD_COR_NEG<=0)) +
               LFill( ORIGEM ));

		      WriteRegistroK275(RegK100.RegistroK270.Items[intFor]);
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK270Count := FRegistroK270Count + RegK100.RegistroK270.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK275(RegK270: TRegistroK270);
var
  intFor: integer;
begin
  if Assigned( RegK270.RegistroK275 ) then
  begin
     for intFor := 0 to RegK270.RegistroK275.Count - 1 do
     begin
        with RegK270.RegistroK275.Items[intFor] do
        begin
          Add( LFill('K275') +
               LFill( COD_ITEM  ) +
               DFill( QTD_COR_POS , 3 ,(QTD_COR_POS<=0)) +
               DFill( QTD_COR_NEG , 3 ,(QTD_COR_NEG<=0)) +
               LFill( COD_INS_SUBST ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK275Count := FRegistroK275Count + RegK270.RegistroK275.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK280(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK280 ) then
  begin
     for intFor := 0 to RegK100.RegistroK280.Count - 1 do
     begin
        with RegK100.RegistroK280.Items[intFor] do
        begin
          if not (DT_EST < RegK100.DT_INI) then
             raise Exception.Create('A data do estoque que está sendo corrigido deve ser anterior à data informada no campo DT_INI do Registro K100');

          Add( LFill('K280') +
               LFill( DT_EST ) +
               LFill( COD_ITEM ) +
               DFill( QTD_COR_POS , 3 ,(QTD_COR_POS<=0)) +
               DFill( QTD_COR_NEG , 3 ,(QTD_COR_NEG<=0)) +
               LFill( Integer(IND_EST), 0 ) +
               LFill( COD_PART ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK280Count := FRegistroK280Count + RegK100.RegistroK280.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK290(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK290 ) then
  begin
     for intFor := 0 to RegK100.RegistroK290.Count - 1 do
     begin
        with RegK100.RegistroK290.Items[intFor] do
        begin
          if ((COD_DOC_OP <> '') or (DT_FIN_OP > 0)) and (DT_INI_OP <= 0) then
            raise Exception.Create('O campo DT_INI_OP será obrigatório conforme informação do campo COD_DOC_OP ou DT_FIN_OP');
          if ((DT_INI_OP > 0) or (DT_FIN_OP > 0)) and (COD_DOC_OP = '') then
            raise Exception.Create('O campo COD_DOC_OP será obrigatório conforme informação do campo DT_INI_OP ou DT_FIN_OP');

          Add( LFill('K290') +
               LFill( DT_INI_OP ) +
               LFill( DT_FIN_OP ) +
               LFill( COD_DOC_OP ));

          WriteRegistroK291(RegK100.RegistroK290.Items[intFor]);
          WriteRegistroK292(RegK100.RegistroK290.Items[intFor]);
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK290Count := FRegistroK290Count + RegK100.RegistroK290.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK291(RegK290: TRegistroK290);
var
  intFor: integer;
begin
  if Assigned( RegK290.RegistroK291 ) then
  begin
     for intFor := 0 to RegK290.RegistroK291.Count - 1 do
     begin
        with RegK290.RegistroK291.Items[intFor] do
        begin
          if (QTD <= 0) then
           raise Exception.Create('Não é admitida quantidade negativa, valor deve ser maior que zero, informado em QTD do Registro K291');

          Add( LFill('K291') +
               LFill( COD_ITEM ) +
               DFill( QTD , 3 ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK291Count := FRegistroK291Count + RegK290.RegistroK291.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK292(RegK290: TRegistroK290);
var
  intFor: integer;
begin
  if Assigned( RegK290.RegistroK292 ) then
  begin
     for intFor := 0 to RegK290.RegistroK292.Count - 1 do
     begin
        with RegK290.RegistroK292.Items[intFor] do
        begin
          if (QTD <= 0) then
           raise Exception.Create('Não é admitida quantidade negativa, valor deve ser maior que zero, informado em QTD do Registro K292');

          Add( LFill('K292') +
               LFill( COD_ITEM ) +
               DFill( QTD , 3 ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK292Count := FRegistroK292Count + RegK290.RegistroK292.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK300(RegK100: TRegistroK100);
var
  intFor: integer;
begin
  if Assigned( RegK100.RegistroK300 ) then
  begin
     for intFor := 0 to RegK100.RegistroK300.Count - 1 do
     begin
        with RegK100.RegistroK300.Items[intFor] do
        begin
          if (DT_PROD > RegK100.DT_FIN) or (DT_PROD < RegK100.DT_INI) then
           raise Exception.Create('A data deve estar compreendida no período informado nos campos DT_INI e DT_FIN do Registro K100');

          Add( LFill('K300') +
               LFill( DT_PROD ));

          WriteRegistroK301(RegK100.RegistroK300.Items[intFor]);
          WriteRegistroK302(RegK100.RegistroK300.Items[intFor]);
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK300Count := FRegistroK300Count + RegK100.RegistroK300.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK301(RegK300: TRegistroK300);
var
  intFor: integer;
begin
  if Assigned( RegK300.RegistroK301 ) then
  begin
     for intFor := 0 to RegK300.RegistroK301.Count - 1 do
     begin
        with RegK300.RegistroK301.Items[intFor] do
        begin
          if (QTD <= 0) then
           raise Exception.Create('Não é admitida quantidade negativa, valor deve ser maior que zero, informado em QTD do Registro K301');

          Add( LFill('K301') +
               LFill( COD_ITEM ) +
               DFill( QTD , 3 ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK301Count := FRegistroK301Count + RegK300.RegistroK301.Count;
  end;
end;

procedure TBloco_K.WriteRegistroK302(RegK300: TRegistroK300);
var
  intFor: integer;
begin
  if Assigned( RegK300.RegistroK302 ) then
  begin
     for intFor := 0 to RegK300.RegistroK302.Count - 1 do
     begin
        with RegK300.RegistroK302.Items[intFor] do
        begin
          if (QTD <= 0) then
           raise Exception.Create('Não é admitida quantidade negativa, valor deve ser maior que zero, informado em QTD do Registro K302');

          Add( LFill('K302') +
               LFill( COD_ITEM ) +
               DFill( QTD , 3 ));
        end;
        RegistroK990.QTD_LIN_K := RegistroK990.QTD_LIN_K + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroK302Count := FRegistroK302Count + RegK300.RegistroK302.Count;
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
  FRegistroK280Count := 0;
  FRegistroK290Count := 0;
  FRegistroK291Count := 0;
  FRegistroK292Count := 0;
  FRegistroK300Count := 0;
  FRegistroK301Count := 0;
  FRegistroK302Count := 0;

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

function TBloco_K.RegistroK210New: TRegistroK210;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K210 deve ser filho do registro K100, e não existe nenhum K100 pai!');

   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK210.New(K100);
end;

function TBloco_K.RegistroK215New: TRegistroK215;
var
   K210: TRegistroK210;
   K100Count: integer;
   K210Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K210Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK210.Count -1;
   if K210Count = -1 then
      raise Exception.Create('O registro K215 deve ser filho do registro K210, e não existe nenhum K210 pai!');

   K210   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK210.Items[K210Count];
   Result := K210.RegistroK215.New(K210);
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

function TBloco_K.RegistroK260New: TRegistroK260;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K260 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK260.New(K100);
end;

function TBloco_K.RegistroK265New: TRegistroK265;
var
   K260: TRegistroK260;
   K100Count: integer;
   K260Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K260Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK260.Count -1;
   if K260Count = -1 then
      raise Exception.Create('O registro K265 deve ser filho do registro K260, e não existe nenhum K260 pai!');

   K260   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK260.Items[K260Count];
   Result := K260.RegistroK265.New(K260);
end;

function TBloco_K.RegistroK270New: TRegistroK270;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K270 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK270.New(K100);
end;

function TBloco_K.RegistroK275New: TRegistroK275;
var
   K270: TRegistroK270;
   K100Count: integer;
   K270Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K270Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK270.Count -1;
   if K270Count = -1 then
      raise Exception.Create('O registro K275 deve ser filho do registro K270, e não existe nenhum K270 pai!');

   K270   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK270.Items[K270Count];
   Result := K270.RegistroK275.New(K270);
end;

function TBloco_K.RegistroK280New: TRegistroK280;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K280 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK280.New(K100);
end;

function TBloco_K.RegistroK290New: TRegistroK290;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K290 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK290.New(K100);
end;

function TBloco_K.RegistroK291New: TRegistroK291;
var
   K290: TRegistroK290;
   K100Count: integer;
   K290Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K290Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK290.Count -1;
   if K290Count = -1 then
      raise Exception.Create('O registro K291 deve ser filho do registro K290, e não existe nenhum K290 pai!');

   K290   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK290.Items[K290Count];
   Result := K290.RegistroK291.New(K290);
end;

function TBloco_K.RegistroK292New: TRegistroK292;
var
   K290: TRegistroK290;
   K100Count: integer;
   K290Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K290Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK290.Count -1;
   if K290Count = -1 then
      raise Exception.Create('O registro K292 deve ser filho do registro K290, e não existe nenhum K290 pai!');

   K290   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK290.Items[K290Count];
   Result := K290.RegistroK292.New(K290);
end;

function TBloco_K.RegistroK300New: TRegistroK300;
var
   K100: TRegistroK100;
   K100Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   if K100Count = -1 then
      raise Exception.Create('O registro K300 deve ser filho do registro K100, e não existe nenhum K100 pai!');
   //
   K100   := FRegistroK001.RegistroK100.Items[K100Count];
   Result := K100.RegistroK300.New(K100);
end;

function TBloco_K.RegistroK301New: TRegistroK301;
var
   K300: TRegistroK300;
   K100Count: integer;
   K300Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K300Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK300.Count -1;
   if K300Count = -1 then
      raise Exception.Create('O registro K301 deve ser filho do registro K300, e não existe nenhum K300 pai!');

   K300   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK300.Items[K300Count];
   Result := K300.RegistroK301.New(K300);
end;

function TBloco_K.RegistroK302New: TRegistroK302;
var
   K300: TRegistroK300;
   K100Count: integer;
   K300Count: integer;
begin
   K100Count := FRegistroK001.RegistroK100.Count -1;
   K300Count := FRegistroK001.RegistroK100.Items[K100Count].RegistroK300.Count -1;
   if K300Count = -1 then
      raise Exception.Create('O registro K302 deve ser filho do registro K300, e não existe nenhum K300 pai!');

   K300   := FRegistroK001.RegistroK100.Items[K100Count].RegistroK300.Items[K300Count];
   Result := K300.RegistroK302.New(K300);
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

