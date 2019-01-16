{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Isaque Pinheiro                      }
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
|* 15/02/2011: Isaque Pinheiro e Fernando Amado
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEPCBloco_F_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEPCBloco_F, ACBrEPCBlocos,
     ACBrEPCBloco_0_Class;

type
  /// TBloco_F -

  { TBloco_F }

  TBloco_F = class(TACBrSPED)
  private
    FRegistroF001                       : TRegistroF001;      /// BLOCO F - RegistroF001
    FRegistroF990                       : TRegistroF990;      /// BLOCO F - RegistroF990

    FRegistroF010Count                  : integer;
    FRegistroF100Count                  : integer;
    FRegistroF111Count                  : integer;
    FRegistroF120Count                  : integer;
    FRegistroF129Count                  : integer;
    FRegistroF130Count                  : integer;
    FRegistroF139Count                  : integer;
    FRegistroF150Count                  : integer;
    FRegistroF200Count                  : integer;
    FRegistroF205Count                  : integer;
    FRegistroF210Count                  : integer;
    FRegistroF211Count                  : integer;
    FRegistroF500Count                  : integer;
    FRegistroF509Count                  : integer;
    FRegistroF510Count                  : integer;
    FRegistroF519Count                  : integer;
    FRegistroF525Count                  : integer;
    FRegistroF550Count                  : integer;
    FRegistroF559Count                  : integer;
    FRegistroF560Count                  : integer;
    FRegistroF569Count                  : integer;
    FRegistroF600Count                  : integer;
    FRegistroF700Count                  : integer;
    FRegistroF800Count                  : integer;
    FBloco_0                            : TBloco_0;

    procedure WriteRegistroF010(RegF001 : TRegistroF001);
    procedure WriteRegistroF100(RegF010 : TRegistroF010);
    procedure WriteRegistroF111(RegF100 : TRegistroF100);
    procedure WriteRegistroF120(RegF010 : TRegistroF010);
    procedure WriteRegistroF129(RegF120 : TRegistroF120);
    procedure WriteRegistroF130(RegF010 : TRegistroF010);
    procedure WriteRegistroF139(RegF130 : TRegistroF130);
    procedure WriteRegistroF150(RegF010 : TRegistroF010);
    procedure WriteRegistroF200(RegF010 : TRegistroF010);
    procedure WriteRegistroF205(RegF200 : TRegistroF200);
    procedure WriteRegistroF210(RegF200 : TRegistroF200);
    procedure WriteRegistroF211(RegF200 : TRegistroF200);
    procedure WriteRegistroF500(RegF010 : TRegistroF010);
    procedure WriteRegistroF510(RegF010 : TRegistroF010);
    procedure WriteRegistroF525(RegF010 : TRegistroF010);
    procedure WriteRegistroF550(RegF010 : TRegistroF010);
    procedure WriteRegistroF560(RegF010 : TRegistroF010);
    procedure WriteRegistroF600(RegF010 : TRegistroF010);
    procedure WriteRegistroF700(RegF010 : TRegistroF010);
    procedure WriteRegistroF800(RegF010 : TRegistroF010);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create ;                                      /// Create
    destructor Destroy; override;                             /// Destroy
    procedure LimpaRegistros; override;

    function RegistroF001New            : TRegistroF001;
    function RegistroF010New            : TRegistroF010;
    function RegistroF100New            : TRegistroF100;
    function RegistroF111New            : TRegistroF111;
    function RegistroF120New            : TRegistroF120;
    function RegistroF129New            : TRegistroF129;
    function RegistroF130New            : TRegistroF130;
    function RegistroF139New            : TRegistroF139;
    function RegistroF150New            : TRegistroF150;
    function RegistroF200New            : TRegistroF200;
    function RegistroF205New            : TRegistroF205;
    function RegistroF210New            : TRegistroF210;
    function RegistroF211New            : TRegistroF211;
    function RegistroF500New            : TRegistroF500;
    function RegistroF509New            : TRegistroF509;
    function RegistroF510New            : TRegistroF510;
    function RegistroF519New            : TRegistroF519;
    function RegistroF525New            : TRegistroF525;
    function RegistroF550New            : TRegistroF550;
    function RegistroF559New            : TRegistroF559;
    function RegistroF560New            : TRegistroF560;
    function RegistroF569New            : TRegistroF569;
    function RegistroF600New            : TRegistroF600;
    function RegistroF700New            : TRegistroF700;
    function RegistroF800New            : TRegistroF800;

    procedure WriteRegistroF001;
    procedure WriteRegistroF990;

    property Bloco_0                    : TBloco_0      read FBloco_0           write FBloco_0;
    property RegistroF001               : TRegistroF001 read FRegistroF001      write FRegistroF001;
    property RegistroF990               : TRegistroF990 read FRegistroF990      write FRegistroF990;

    property RegistroF010Count          : integer       read FRegistroF010Count write FRegistroF010Count;
    property RegistroF100Count          : integer       read FRegistroF100Count write FRegistroF100Count;
    property RegistroF111Count          : integer       read FRegistroF111Count write FRegistroF111Count;
    property RegistroF120Count          : integer       read FRegistroF120Count write FRegistroF120Count;
    property RegistroF129Count          : integer       read FRegistroF129Count write FRegistroF129Count;
    property RegistroF130Count          : integer       read FRegistroF130Count write FRegistroF130Count;
    property RegistroF139Count          : integer       read FRegistroF139Count write FRegistroF139Count;
    property RegistroF150Count          : integer       read FRegistroF150Count write FRegistroF150Count;
    property RegistroF200Count          : integer       read FRegistroF200Count write FRegistroF200Count;
    property RegistroF205Count          : integer       read FRegistroF205Count write FRegistroF205Count;
    property RegistroF210Count          : integer       read FRegistroF210Count write FRegistroF210Count;
    property RegistroF211Count          : integer       read FRegistroF211Count write FRegistroF211Count;
    property RegistroF500Count          : integer       read FRegistroF500Count write FRegistroF500Count;
    property RegistroF509Count          : integer       read FRegistroF509Count write FRegistroF509Count;
    property RegistroF510Count          : integer       read FRegistroF510Count write FRegistroF510Count;
    property RegistroF519Count          : integer       read FRegistroF519Count write FRegistroF519Count;
    property RegistroF525Count          : integer       read FRegistroF525Count write FRegistroF525Count;
    property RegistroF550Count          : integer       read FRegistroF550Count write FRegistroF550Count;
    property RegistroF559Count          : integer       read FRegistroF559Count write FRegistroF559Count;
    property RegistroF560Count          : integer       read FRegistroF560Count write FRegistroF560Count;
    property RegistroF569Count          : integer       read FRegistroF569Count write FRegistroF569Count;
    property RegistroF600Count          : integer       read FRegistroF600Count write FRegistroF600Count;
    property RegistroF700Count          : integer       read FRegistroF700Count write FRegistroF700Count;
    property RegistroF800Count          : integer       read FRegistroF800Count write FRegistroF800Count;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_F }

constructor TBloco_F.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_F.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_F.CriaRegistros;
begin
  FRegistroF001           := TRegistroF001.Create;
  FRegistroF990           := TRegistroF990.Create;

  FRegistroF010Count      := 0;
  FRegistroF100Count      := 0;
  FRegistroF111Count      := 0;
  FRegistroF120Count      := 0;
  FRegistroF129Count      := 0;
  FRegistroF130Count      := 0;
  FRegistroF139Count      := 0;
  FRegistroF150Count      := 0;
  FRegistroF200Count      := 0;
  FRegistroF205Count      := 0;
  FRegistroF210Count      := 0;
  FRegistroF211Count      := 0;
  FRegistroF500Count      := 0;
  FRegistroF510Count      := 0;
  FRegistroF525Count      := 0;
  FRegistroF550Count      := 0;
  FRegistroF560Count      := 0;
  FRegistroF600Count      := 0;
  FRegistroF700Count      := 0;
  FRegistroF800Count      := 0;

  FRegistroF990.QTD_LIN_F := 0;
end;

procedure TBloco_F.LiberaRegistros;
begin
  FRegistroF001.Free;
  FRegistroF990.Free;
end;

procedure TBloco_F.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_F.RegistroF001New: TRegistroF001;
begin
   Result := FRegistroF001;
end;

function TBloco_F.RegistroF010New: TRegistroF010;
begin
   Result := FRegistroF001.RegistroF010.New;
end;

function TBloco_F.RegistroF100New: TRegistroF100;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF100.New;
end;

function TBloco_F.RegistroF111New: TRegistroF111;
  var
    F010Count: integer;
    F100Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F100Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF100.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF100.Items[F100Count].RegistroF111.New;
end;

function TBloco_F.RegistroF120New: TRegistroF120;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF120.New;
end;

function TBloco_F.RegistroF129New: TRegistroF129;
  var
    F010Count: integer;
    F120Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F120Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF120.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF120.Items[F120Count].RegistroF129.New;
end;

function TBloco_F.RegistroF130New: TRegistroF130;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF130.New;
end;

function TBloco_F.RegistroF139New: TRegistroF139;
  var
    F010Count: integer;
    F130Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F130Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF130.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF130.Items[F130Count].RegistroF139.New;
end;

function TBloco_F.RegistroF150New: TRegistroF150;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF150.New;
end;

function TBloco_F.RegistroF200New: TRegistroF200;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF200.New;
end;

function TBloco_F.RegistroF205New: TRegistroF205;
  var
    F010Count: integer;
    F200Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F200Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF200.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF200.Items[F200Count].RegistroF205;
end;

function TBloco_F.RegistroF210New: TRegistroF210;
  var
    F010Count: integer;
    F200Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F200Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF200.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF200.Items[F200Count].RegistroF210.New;
end;

function TBloco_F.RegistroF211New: TRegistroF211;
  var
    F010Count: integer;
    F200Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F200Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF200.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF200.Items[F200Count].RegistroF211.New;
end;

(*Por: Edilson Alves de oliveira
       22/11/2011*)
function TBloco_F.RegistroF500New: TRegistroF500;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF500.New;
end;

function TBloco_F.RegistroF509New: TRegistroF509;
  var
    F010Count: integer;
    F500Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F500Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF500.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF500.Items[F500Count].RegistroF509.New;
end;

function TBloco_F.RegistroF550New: TRegistroF550;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF550.New;
end;

function TBloco_F.RegistroF559New: TRegistroF559;
  var
    F010Count: integer;
    F550Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F550Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF550.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF550.Items[F550Count].RegistroF559.New;
end;

function TBloco_F.RegistroF600New: TRegistroF600;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF600.New;
end; 

function TBloco_F.RegistroF700New: TRegistroF700;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF700.New;
end;

function TBloco_F.RegistroF800New: TRegistroF800;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF800.New;
end;

procedure TBloco_F.WriteRegistroF001 ;
begin
  if Assigned(FRegistroF001) then
  begin
     with FRegistroF001 do
     begin
        Add( LFill( 'F001' ) +
             LFill( Integer(IND_MOV), 0 ) ) ;

        if IND_MOV = imComDados then
        begin
          WriteRegistroF010(FRegistroF001) ;
        end;
     end;

     RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
  end;
end;

procedure TBloco_F.WriteRegistroF010(RegF001: TRegistroF001) ;
  var
    intFor: Integer;
begin
  if Assigned(RegF001.RegistroF010) then
  begin
    for intFor := 0 to RegF001.RegistroF010.Count - 1 do
    begin
      with RegF001.RegistroF010.Items[intFor] do
      begin
        Check(funChecaCNPJ(CNPJ), '(F-010) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);

        Add( LFill('F010') +
             LFill(CNPJ, 14) ) ;
      end;

      // Registros FILHOS
      WriteRegistroF100( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF120( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF130( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF150( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF200( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF500( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF510( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF525( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF550( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF560( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF600( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF700( RegF001.RegistroF010.Items[intFor] );
      WriteRegistroF800( RegF001.RegistroF010.Items[intFor] );
      //
      RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroF010Count := FRegistroF010Count + RegF001.RegistroF010.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF100(RegF010: TRegistroF010) ;
  var
    intFor           : integer;
    strIND_OPER      : String;
    strIND_ORIG_CRED : String;
    strNAT_BC_CRED   : String;
begin
  if Assigned(RegF010.RegistroF100) then
  begin
     for intFor := 0 to RegF010.RegistroF100.Count - 1 do
     begin
        with RegF010.RegistroF100.Items[intFor] do
        begin

          case IND_OPER of
            indRepCustosDespesasEncargos : strIND_OPER := '0'; //0 – Operação Representativa de Aquisição, Custos, Despesa ou Encargos, Sujeita à Incidência de Crédito de PIS/Pasep ou Cofins (CST 50 a 66).
            indRepReceitaAuferida        : strIND_OPER := '1'; //1 – Operação Representativa de Receita Auferida Sujeita ao Pagamento da Contribuição para o PIS/Pasep e da Cofins (CST 01, 02, 03 ou 05).
            indRepReceitaNaoAuferida     : strIND_OPER := '2'; //2 - Operação Representativa de Receita Auferida Não Sujeita ao Pagamento da Contribuição para o PIS/Pasep e da Cofins (CST 04, 06, 07, 08, 09, 49 ou 99).

          end;

          case IND_ORIG_CRED of
            opcMercadoInterno : strIND_ORIG_CRED := '0';
            opcImportacao     : strIND_ORIG_CRED := '1';
          end;

          case NAT_BC_CRED of
            bccAqBensRevenda                 : strNAT_BC_CRED := '01';
            bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
            bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
            bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
            bccAluguelPredios                : strNAT_BC_CRED := '05';
            bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
            bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
            bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
            bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
            bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
            bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
            bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
            bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
            bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
            bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
            bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
            bccAtPresServ                    : strNAT_BC_CRED := '17';
            bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
          end;

          Add( LFill('F100')             +
               LFill( strIND_OPER )      +
               LFill( COD_PART )         +
               LFill( COD_ITEM )         +
               LFill( DT_OPER )          +
               LFill( VL_OPER,0,2 )      +
               LFill( CstPisToStr(CST_PIS) )       +
               LFill( VL_BC_PIS,0,4, False, '0','0.0000' )    +
               LFill( ALIQ_PIS,0,4, False, '0','0.0000' )     +
               LFill( VL_PIS,0,2 )       +
               LFill( CstCofinsToStr(CST_COFINS) )    +
               LFill( VL_BC_COFINS,0,4, False, '0','0.0000' ) +
               LFill( ALIQ_COFINS,0,4, False, '0','0.0000' )  +
               LFill( VL_COFINS,0,2 )    +
               LFill( strNAT_BC_CRED )   +
               LFill( strIND_ORIG_CRED ) +
               LFill( COD_CTA )          +
               LFill( COD_CCUS )         +
               LFill( DESC_DOC_OPER ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroF111( RegF010.RegistroF100.Items[intFor] );
        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF100Count := FRegistroF100Count + RegF010.RegistroF100.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF111(RegF100: TRegistroF100) ;
  var
    intFor      : integer;
    strIND_PROC : String;
begin
  if Assigned(RegF100.RegistroF111) then
  begin
    for intFor := 0 to RegF100.RegistroF111.Count - 1 do
    begin
      with RegF100.RegistroF111.Items[intFor] do
      begin
        case IND_PROC of
          opJusticaFederal : strIND_PROC := '1';
          opSecexRFB       : strIND_PROC := '3';
          opOutros         : strIND_PROC := '9';
          opNenhum         : strIND_PROC := '';
        end;

        Add( LFill('F111')   +
             LFill(NUM_PROC) +
             LFill(strIND_PROC) ) ;
      end;

      RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroF111Count := FRegistroF111Count + RegF100.RegistroF111.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF120(RegF010: TRegistroF010) ;
  var
    intFor         : integer;
    strNAT_BC_CRED : String;
begin
  if Assigned(RegF010.RegistroF120) then
  begin
     for intFor := 0 to RegF010.RegistroF120.Count - 1 do
     begin
        with RegF010.RegistroF120.Items[intFor] do
        begin
          case NAT_BC_CRED of
            bccAqBensRevenda                 : strNAT_BC_CRED := '01';
            bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
            bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
            bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
            bccAluguelPredios                : strNAT_BC_CRED := '05';
            bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
            bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
            bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
            bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
            bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
            bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
            bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
            bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
            bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
            bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
            bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
            bccAtPresServ                    : strNAT_BC_CRED := '17';
            bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
          end;

          Add( LFill('F120')                      +
               LFill( strNAT_BC_CRED )            +
               LFill( IDENT_BEM_IMOB )            +    //Verificar criação da tabela no ACBrEPCBlocos
               LFill( IND_ORIG_CRED )             +     //Verificar criação da tabela no ACBrEPCBlocos
               LFill( IND_UTIL_BEM_IMOB )         + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( VL_OPER_DEP,0,2 )           +
               LFill( PARC_OPER_NAO_BC_CRED,0,2 ) +
               LFill( CstPisToStr(CST_PIS) )                +
               LFill( VL_BC_PIS,0,2 )             +
               DFill( ALIQ_PIS, 4 )              +
               LFill( VL_PIS,0,2 )                +
               LFill( CstCofinsToStr(CST_COFINS) )             +
               LFill( VL_BC_COFINS,0,2 )          +
               DFill( ALIQ_COFINS, 4 )           +
               LFill( VL_COFINS,0,2 )             +
               LFill( COD_CTA )                   +
               LFill( COD_CCUS )                  +
               LFill( DESC_BEM_IMOB ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroF129( RegF010.RegistroF120.Items[intFor] );
        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF120Count := FRegistroF120Count + RegF010.RegistroF120.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF129(RegF120: TRegistroF120) ;
  var
    intFor      : integer;
    strIND_PROC : String;
begin
  if Assigned(RegF120.RegistroF129) then
  begin
    for intFor := 0 to RegF120.RegistroF129.Count - 1 do
    begin
      with RegF120.RegistroF129.Items[intFor] do
      begin
        case IND_PROC of
          opJusticaFederal : strIND_PROC := '1';
          opSecexRFB       : strIND_PROC := '3';
          opOutros         : strIND_PROC := '9';
          opNenhum         : strIND_PROC := '';
        end;

        Add( LFill('F129')   +
             LFill(NUM_PROC) +
             LFill(strIND_PROC) ) ;
      end;

      RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroF129Count := FRegistroF129Count + RegF120.RegistroF129.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF130(RegF010: TRegistroF010) ;
  var
    intFor         : integer;
    strNAT_BC_CRED : String;
begin
  if Assigned(RegF010.RegistroF130) then
  begin
     for intFor := 0 to RegF010.RegistroF130.Count - 1 do
     begin
        with RegF010.RegistroF130.Items[intFor] do
        begin
          case NAT_BC_CRED of
            bccAqBensRevenda                 : strNAT_BC_CRED := '01';
            bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
            bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
            bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
            bccAluguelPredios                : strNAT_BC_CRED := '05';
            bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
            bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
            bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
            bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
            bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
            bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
            bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
            bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
            bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
            bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
            bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
            bccAtPresServ                    : strNAT_BC_CRED := '17';
            bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
          end;

          Add( LFill('F130')                      +
               LFill( strNAT_BC_CRED )            +
               LFill( IDENT_BEM_IMOB )            +    //Verificar criação da tabela no ACBrEPCBlocos
               LFill( IND_ORIG_CRED )             +     //Verificar criação da tabela no ACBrEPCBlocos
               LFill( IND_UTIL_BEM_IMOB )         + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( MES_OPER_AQUIS )            +
               LFill( VL_OPER_AQUIS,0,2 )         +
               LFill( PARC_OPER_NAO_BC_CRED,0,2 ) +
               LFill( VL_BC_CRED,0,2 )            +
               LFill( IND_NR_PARC,1 )             +
               LFill( CstPisToStr(CST_PIS) )      +
               LFill( VL_BC_PIS,0,2 )             +
               DFill( ALIQ_PIS, 4 )               +
               LFill( VL_PIS,0,2 )                +
               LFill( CstCofinsToStr(CST_COFINS) ) +
               LFill( VL_BC_COFINS,0,2 )          +
               DFill( ALIQ_COFINS, 4 )            +
               LFill( VL_COFINS,0,2 )             +
               LFill( COD_CTA )                   +
               LFill( COD_CCUS )                  +
               LFill( DESC_BEM_IMOB ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroF139( RegF010.RegistroF130.Items[intFor] );
        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF130Count := FRegistroF130Count + RegF010.RegistroF130.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF139(RegF130: TRegistroF130) ;
  var
    intFor      : integer;
    strIND_PROC : String;
begin
  if Assigned(RegF130.RegistroF139) then
  begin
    for intFor := 0 to RegF130.RegistroF139.Count - 1 do
    begin
      with RegF130.RegistroF139.Items[intFor] do
      begin
        case IND_PROC of
          opJusticaFederal : strIND_PROC := '1';
          opSecexRFB       : strIND_PROC := '3';
          opOutros         : strIND_PROC := '9';
          opNenhum         : strIND_PROC := '';
        end;

        Add( LFill('F139')   +
             LFill(NUM_PROC) +
             LFill(strIND_PROC) ) ;
      end;

      RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroF139Count := FRegistroF139Count + RegF130.RegistroF139.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF150(RegF010: TRegistroF010) ;
  var
    intFor         : integer;
    strNAT_BC_CRED : String;
begin
  if Assigned(RegF010.RegistroF150) then
  begin
     for intFor := 0 to RegF010.RegistroF150.Count - 1 do
     begin
        with RegF010.RegistroF150.Items[intFor] do
        begin
          case NAT_BC_CRED of
            bccAqBensRevenda                 : strNAT_BC_CRED := '01';
            bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
            bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
            bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
            bccAluguelPredios                : strNAT_BC_CRED := '05';
            bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
            bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
            bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
            bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
            bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
            bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
            bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
            bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
            bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
            bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
            bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
            bccAtPresServ                    : strNAT_BC_CRED := '17';
            bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
          end;

          Add( LFill('F150')               +
               LFill( strNAT_BC_CRED )     +
               LFill( VL_TOT_EST,0,2 )     +
               LFill( EST_IMP,0,2 )        +
               LFill( VL_BC_EST,0,2 )      +
               LFill( VL_BC_MEN_EST,0,2 )  +
               LFill( CstPisToStr(CST_PIS) )  +
               DFill( ALIQ_PIS, 4 )       +
               LFill( VL_CRED_PIS,0,2 )    +
               LFill( CstCofinsToStr(CST_COFINS) ) +
               DFill( ALIQ_COFINS, 4 )    +
               LFill( VL_CRED_COFINS,0,2 ) +
               LFill( DESC_EST )           +
               LFill( COD_CTA ) ) ;
        end;

        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF150Count := FRegistroF150Count + RegF010.RegistroF150.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF200(RegF010: TRegistroF010) ;
  var
    intFor         : integer;
//    strIND_OPER    : String;
begin
  if Assigned(RegF010.RegistroF200) then
  begin
     for intFor := 0 to RegF010.RegistroF200.Count - 1 do
     begin
        with RegF010.RegistroF200.Items[intFor] do
        begin
//          case IND_OPER of
//            itoContratado : strIND_OPER := '0';
//            itoPrestado   : strIND_OPER := '1';
//          end;
          Add( LFill('F200')               +
               LFill( IND_OPER )           + //Verificar criação da tabela no ACBrEPCBlocos ATENÇÃO: não é igual a TACBrIndicadorTpOperacao
               LFill( UNID_IMOB )          + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( IDENT_EMP )          +
               LFill( DESC_UNID_IMOB )     +
               LFill( NUM_CONT )           +
               LFill( CPF_CNPJ_ADQU )      +
               LFill( DT_OPER )            +
               LFill( VL_TOT_VEND,0,2 )    +
               LFill( VL_REC_ACUM,0,2 )    +
               LFill( VL_TOT_REC,0,2 )     +
               LFill( CstPisToStr(CST_PIS) ) +
               LFill( VL_BC_PIS,0,2 )      +
               DFill( ALIQ_PIS, 4 )       +
               LFill( VL_PIS,0,2 )         +
               LFill( CstCofinsToStr(CST_COFINS) )      +
               LFill( VL_BC_COFINS,0,2 )   +
               DFill( ALIQ_COFINS, 4 )    +
               LFill( VL_COFINS,0,2 )      +
               LFill( PERC_REC_RECEB,0,2 ) +
               LFill( IND_NAT_EMP )        + //Verificar criação da tabela no ACBrEPCBlocos
               LFill( INF_COMP ) ) ;
        end;

        // Registros FILHOS
       if (Bloco_0.Registro0001.Registro0110.COD_INC_TRIB <> codEscrOpIncCumulativo) then
          WriteRegistroF205( RegF010.RegistroF200.Items[intFor] );
        WriteRegistroF210( RegF010.RegistroF200.Items[intFor] );
        WriteRegistroF211( RegF010.RegistroF200.Items[intFor] );
        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF200Count := FRegistroF200Count + RegF010.RegistroF200.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF205(RegF200: TRegistroF200) ;
begin
  if Assigned(RegF200.RegistroF205) then
  begin
    with RegF200.RegistroF205 do
    begin
      Add( LFill('F205')                        +
           LFill( VL_CUS_INC_ACUM_ANT,0,2 )     +
           LFill( VL_CUS_INC_PER_ESC,0,2 )      +
           LFill( VL_CUS_INC_ACUM,0,2 )         +
           LFill( VL_EXC_BC_CUS_INC_ACUM,0,2 )  +
           LFill( VL_BC_CUS_INC,0,2 )           +
           LFill( CstPisToStr(CST_PIS) )                  +
           DFill( ALIQ_PIS, 4 )                +
           LFill( VL_CRED_PIS_ACUM,0,2 )        +
           LFill( VL_CRED_PIS_DESC_ANT,0,2 )    +
           LFill( VL_CRED_PIS_DESC,0,2 )        +
           LFill( VL_CRED_PIS_DESC_FUT,0,2 )    +
           LFill( CstCofinsToStr(CST_COFINS) )               +
           DFill( ALIQ_COFINS, 4 )             +
           LFill( VL_CRED_COFINS_ACUM,0,2 )     +
           LFill( VL_CRED_COFINS_DESC_ANT,0,2 ) +
           LFill( VL_CRED_COFINS_DESC,0,2 )     +
           LFill( VL_CRED_COFINS_DESC_FUT,0,2 ) ) ;
      //
      RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroF205Count := FRegistroF205Count + 1;
  end;
end;

procedure TBloco_F.WriteRegistroF210(RegF200: TRegistroF200) ;
  var
    intFor         : integer;
begin
  if Assigned(RegF200.RegistroF210) then
  begin
    for intFor := 0 to RegF200.RegistroF210.Count - 1 do
    begin
      with RegF200.RegistroF210.Items[intFor] do
      begin
        Add( LFill('F210')                 +
             LFill( VL_CUS_ORC,0,2 )       +
             LFill( VL_EXC,0,2 )           +
             LFill( VL_CUS_ORC_AJU,0,2 )   +
             LFill( VL_BC_CRED,0,2 )       +
             LFill( CstPisToStr(CST_PIS) ) +
             DFill( ALIQ_PIS, 4 )          +
             LFill( VL_CRED_PIS_UTIL,0,2 ) +
             LFill( CstCofinsToStr(CST_COFINS) ) +
             DFill( ALIQ_COFINS, 4 )      +
             LFill( VL_CRED_COFINS_UTIL,0,2 ) ) ;
        //
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroF210Count := FRegistroF210Count + RegF200.RegistroF210.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF211(RegF200: TRegistroF200) ;
  var
    intFor      : integer;
    strIND_PROC : String;
begin
  if Assigned(RegF200.RegistroF211) then
  begin
    for intFor := 0 to RegF200.RegistroF211.Count - 1 do
    begin
      with RegF200.RegistroF211.Items[intFor] do
      begin
        case IND_PROC of
          opJusticaFederal : strIND_PROC := '1';
          opSecexRFB       : strIND_PROC := '3';
          opOutros         : strIND_PROC := '9';
          opNenhum         : strIND_PROC := '';
        end;

        Add( LFill('F211')   +
             LFill(NUM_PROC) +
             LFill(strIND_PROC) ) ;
      end;

      RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroF211Count := FRegistroF211Count + RegF200.RegistroF211.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF550(RegF010: TRegistroF010) ;
  var
    intFor, intF559 : integer;
begin
  //(*) Os registros referentes à escrituração do PIS/Pasep e da Cofins das pessoas jurídicas sujeitas ao regime de tributação
  //com  base  no  lucro  presumido,  aplicável  para  os  fatos  geradores  a  ocorrer  a  partir  de  01  de  janeiro  de  2012,  serão
  //disponibilizados  pelo  Programa  Validador  e  Assinador  (PVA)  da  EFD-PIS/Cofins,  versão  1.05,  com  previsão  de
  //disponibilização pela Receita Federal em janeiro/2012.
  if DT_INI >= EncodeDate(2012,01,01) then
  begin
     if Assigned(RegF010.RegistroF550) then
     begin
        for intFor := 0 to RegF010.RegistroF550.Count - 1 do
        begin
           with RegF010.RegistroF550.Items[intFor] do
           begin
             Add( LFill('F550')                +
                  LFill( VL_REC_COMP,0,2 )     +
                  LFill( CstPisToStr(CST_PIS) ) +
                  LFill( VL_DESC_PIS ,0,2 )    +
                  DFill( VL_BC_PIS, 2 )        +
                  DFill( ALIQ_PIS, 4 )         +
                  LFill( VL_PIS,0,2 )          +
                  LFill( CstCofinsToStr(CST_COFINS) ) +
                  LFill( VL_DESC_COFINS ,0,2 ) +
                  DFill( VL_BC_COFINS, 2 )     +
                  DFill( ALIQ_COFINS, 4 )      +
                  LFill( VL_COFINS,0,2 )       +
                  LFill( COD_MOD )             +
                  LFill( CFOP, 4, True )       +
                  LFill( COD_CTA )             +
                  LFill( INFO_COMPL ) ) ;
           end;

           for intF559 := 0 to RegF010.RegistroF550.Items[intFor].RegistroF559.Count -1 do
           begin
               with RegF010.RegistroF550.Items[intFor].RegistroF559.Items[intF559] do
               begin
                   Add( LFill('F559') +
                     LFill( NUM_PROC, 20) +
                     LFill( OrigemProcessoToStr(IND_PROC), 1));
               end;
               RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
           end;
           FRegistroF559Count := FRegistroF559Count + RegF010.RegistroF550.Items[intFor].RegistroF559.Count;
           ///
           RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
        end;
        /// Variavél para armazenar a quantidade de registro do tipo.
        FRegistroF550Count := FRegistroF550Count + RegF010.RegistroF550.Count;
     end;
  end;
end;


procedure TBloco_F.WriteRegistroF560(RegF010: TRegistroF010) ;
  var
    intFor : integer;
begin
  //(*) Os registros referentes à escrituração do PIS/Pasep e da Cofins das pessoas jurídicas sujeitas ao regime de tributação
  //com  base  no  lucro  presumido,  aplicável  para  os  fatos  geradores  a  ocorrer  a  partir  de  01  de  janeiro  de  2012,  serão
  //disponibilizados  pelo  Programa  Validador  e  Assinador  (PVA)  da  EFD-PIS/Cofins,  versão  1.05,  com  previsão  de
  //disponibilização pela Receita Federal em janeiro/2012.
  //if DT_INI >= EncodeDate(2012,01,01) then
  //begin
     if Assigned(RegF010.RegistroF560) then
     begin
        for intFor := 0 to RegF010.RegistroF560.Count - 1 do
        begin
           with RegF010.RegistroF560.Items[intFor] do
           begin
             Add( LFill('F560')                   +
                  LFill( VL_REC_COMP      ,0,2 )  +
                  LFill( CstPisToStr(CST_PIS)     ) +
                  LFill( VL_DESC_PIS       ,0,2 ) +
                  DFill( QUANT_BC_PIS      , 3 )  +
                  DFill( ALIQ_PIS_QUANT    , 4 )  +
                  LFill( VL_PIS           ,0,2 )  +
                  LFill( CstCofinsToStr(CST_COFINS) )   +
                  LFill( VL_DESC_COFINS    ,0,2 ) +
                  DFill( QUANT_BC_COFINS   , 3 )  +
                  DFill( ALIQ_COFINS_QUANT , 4 )  +
                  LFill( VL_COFINS         ,0,2 ) +
                  LFill( COD_MOD           )      +
                  LFill( CFOP, 4, True     )      +
                  LFill( COD_CTA           )      +
                  LFill( INFO_COMPL        ) )    ;
           end;
           ///
           RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
        end;
        /// Variavél para armazenar a quantidade de registro do tipo.
        FRegistroF560Count := FRegistroF560Count + RegF010.RegistroF560.Count;
     end;
  //end;
end;
procedure TBloco_F.WriteRegistroF600(RegF010: TRegistroF010) ;
  var
    intFor         : integer;
    strIND_NAT_REC, strIND_NAT_RET : String;
begin
  if Assigned(RegF010.RegistroF600) then
  begin
     for intFor := 0 to RegF010.RegistroF600.Count - 1 do
     begin
        with RegF010.RegistroF600.Items[intFor] do
        begin

          case IND_NAT_REC of
            inrNaoCumulativa : strIND_NAT_REC := '0' ; // 0 // Receita de Natureza Não Cumulativa
            inrCumulativa    : strIND_NAT_REC := '1' ; // 1 // Receita de Natureza Cumulativa
            inrNenhum        : strIND_NAT_REC := '' ;  // Vazio
          end;

          case IND_NAT_RET of
            indRetOrgAutarquiasFundFederais : strIND_NAT_RET := '01' ; // 01 - Retenção por Órgãos, Autarquias e Fundações Federais
            indRetEntAdmPublicaFederal      : strIND_NAT_RET := '02' ; // 02 - Retenção por outras Entidades da Administração Pública Federal
            indRetPesJuridicasDireitoPri    : strIND_NAT_RET := '03' ; // 03 - Retenção por Pessoas Jurídicas de Direito Privado
            indRecolhimentoSociedadeCoop    : strIND_NAT_RET := '04' ; // 04 - Recolhimento por Sociedade Cooperativa
            indRetFabricanteMaqVeiculos     : strIND_NAT_RET := '05' ; // 05 - Retenção por Fabricante de Máquinas e Veículos
            indOutrasRetencoes              : strIND_NAT_RET := '99' ; // 99 - Outras Retenções
          end;

          Add( LFill('F600')               +
               LFill( strIND_NAT_RET )     +
               LFill( DT_RET )             +
               DFill( VL_BC_RET , 4 )     +
               LFill( VL_RET ,0,2 )        +
               LFill( COD_REC )            +
               LFill( strIND_NAT_REC )     +
               LFill( CNPJ )               +
               LFill( VL_RET_PIS ,0,2 )    +
               LFill( VL_RET_COFINS ,0,2 ) +
               LFill( IND_DEC ) ) ;
        end;

        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF600Count := FRegistroF600Count + RegF010.RegistroF600.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF700(RegF010: TRegistroF010) ;
  var
    intFor: integer;
    strIND_ORI_DED, strIND_NAT_DED: String;
begin
  if Assigned(RegF010.RegistroF700) then
  begin
     for intFor := 0 to RegF010.RegistroF700.Count - 1 do
     begin
        with RegF010.RegistroF700.Items[intFor] do
        begin
        
          case IND_ORI_DED of
             indCredPreMed              : strIND_ORI_DED := '01' ; // 01 – Créditos Presumidos - Medicamentos
             indCredAdmRegCumulativoBeb : strIND_ORI_DED := '02' ; // 02 – Créditos Admitidos no Regime Cumulativo – Bebidas Frias
             indContribSTZFM            : strIND_ORI_DED := '03' ; // 03 – Contribuição Paga pelo Substituto Tributário - ZFM
             indSTNaoOCFatoGeradorPres  : strIND_ORI_DED := '04' ; // 04 – Substituição Tributária – Não Ocorrência do Fato Gerador Presumido
             indOutrasDeducoes          : strIND_ORI_DED := '99' ; // 99 - Outras Deduções
          end;

          case IND_NAT_DED of
            indNaoAcumulativa  : strIND_NAT_DED := '0' ;  // 0 – Dedução de Natureza Não Cumulativa
            indAcumulativa     : strIND_NAT_DED := '1' ;  // 1 – Dedução de Natureza Cumulativa
          end;

          Add( LFill('F700')               +
               LFill( strIND_ORI_DED )     +
               LFill( strIND_NAT_DED )     +
               LFill( VL_DED_PIS ,0,2 )    +
               LFill( VL_DED_COFINS ,0,2 ) +
               LFill( VL_BC_OPER ,0,2 )    +
               LFill( CNPJ )               +
               LFill( INF_COMP ) ) ;
        end;

        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF700Count := FRegistroF700Count + RegF010.RegistroF700.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF800(RegF010: TRegistroF010) ;
  var
    intFor      : integer;
    strCOD_CRED : String;
begin
  if Assigned(RegF010.RegistroF800) then
  begin
     for intFor := 0 to RegF010.RegistroF800.Count - 1 do
     begin
        with RegF010.RegistroF800.Items[intFor] do
        begin

          case COD_CRED of
                    //CÓDIGOS VINCULADOS À RECEITA TRIBUTADA NO MERCADO INTERNO - Grupo 100
                    ccRTMIAliqBasica         : strCOD_CRED := '101' ; // Crédito vinculado à receita tributada no mercado interno - Alíquota Básica
                    ccRTMIAliqDiferenciada   : strCOD_CRED := '102' ; // Crédito vinculado à receita tributada no mercado interno - Alíquotas Diferenciadas
                    ccRTMIAliqUnidProduto    : strCOD_CRED := '103' ; // Crédito vinculado à receita tributada no mercado interno - Alíquota por Unidade de Produto
                    ccRTMIEstAbertura        : strCOD_CRED := '104' ; // Crédito vinculado à receita tributada no mercado interno - Estoque de Abertura
                    ccRTMIAquiEmbalagem      : strCOD_CRED := '105' ; // Crédito vinculado à receita tributada no mercado interno - Aquisição Embalagens para revenda
                    ccRTMIPreAgroindustria   : strCOD_CRED := '106' ; // Crédito vinculado à receita tributada no mercado interno - Presumido da Agroindústria
                    ccRTMIImportacao         : strCOD_CRED := '108' ; // Crédito vinculado à receita tributada no mercado interno - Importação
                    ccRTMIAtivImobiliaria    : strCOD_CRED := '109' ; // Crédito vinculado à receita tributada no mercado interno - Atividade Imobiliária
                    ccRTMIOutros             : strCOD_CRED := '199' ; // Crédito vinculado à receita tributada no mercado interno - Outros
                    //CÓDIGOS VINCULADOS À RECEITA NÃO TRIBUTADA NO MERCADO INTERNO - Grupo 200
                    ccRNTMIAliqBasica        : strCOD_CRED := '201' ; // Crédito vinculado à receita não tributada no mercado interno - Alíquota Básica
                    ccRNTMIAliqDiferenciada  : strCOD_CRED := '202' ; // Crédito vinculado à receita não tributada no mercado interno - Alíquotas Diferenciadas
                    ccRNTMIAliqUnidProduto   : strCOD_CRED := '203' ; // Crédito vinculado à receita não tributada no mercado interno - Alíquota por Unidade de Produto
                    ccRNTMIEstAbertura       : strCOD_CRED := '204' ; // Crédito vinculado à receita não tributada no mercado interno - Estoque de Abertura
                    ccRNTMIAquiEmbalagem     : strCOD_CRED := '205' ; // Crédito vinculado à receita não tributada no mercado interno - Aquisição Embalagens para revenda
                    ccRNTMIPreAgroindustria  : strCOD_CRED := '206' ; // Crédito vinculado à receita não tributada no mercado interno - Presumido da Agroindústria
                    ccRNTMIImportacao        : strCOD_CRED := '208' ; // Crédito vinculado à receita não tributada no mercado interno - Importação
                    ccRNTMIOutros            : strCOD_CRED := '299' ; // Crédito vinculado à receita não tributada no mercado interno - Outros
                    //CÓDIGOS VINCULADOS À RECEITA DE EXPORTAÇÃO - Grupo 300
                    ccREAliqBasica           : strCOD_CRED := '301' ; // Crédito vinculado à receita de exportação - Alíquota Básica
                    ccREAliqDiferenciada     : strCOD_CRED := '302' ; // Crédito vinculado à receita de exportação - Alíquotas Diferenciadas
                    ccREAliqUnidProduto      : strCOD_CRED := '303' ; // Crédito vinculado à receita de exportação - Alíquota por Unidade de Produto
                    ccREEstAbertura          : strCOD_CRED := '304' ; // Crédito vinculado à receita de exportação - Estoque de Abertura
                    ccREAquiEmbalagem        : strCOD_CRED := '305' ; // Crédito vinculado à receita de exportação - Aquisição Embalagens para revenda
                    ccREPreAgroindustria     : strCOD_CRED := '306' ; // Crédito vinculado à receita de exportação - Presumido da Agroindústria
                    ccREPreAgroindustriaPCR  : strCOD_CRED := '307' ; // Crédito vinculado à receita de exportação - Presumido da Agroindústria – Passível de Compensação e/ou Ressarcimento
                    ccREImportacao           : strCOD_CRED := '308' ; // Crédito vinculado à receita de exportação - Importação
                    ccREOutros               : strCOD_CRED := '399' ; // Crédito vinculado à receita de exportação - Outros
          end;

          Add( LFill('F800')                +
               LFill( IND_NAT_EVEN )        +   //Verificar criação da tabela no ACBrEPCBlocos
               LFill( DT_EVEN )             +
               LFill( CNPJ_SUCED )          +
               LFill( PA_CONT_CRED )        +
               LFill( strCOD_CRED )         +   
               LFill( VL_CRED_PIS ,0,2 )    +
               LFill( VL_CRED_COFINS ,0,2 ) +
               LFill( PER_CRED_CIS ,0,2 ) ) ;
        end;
        ///
        RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroF800Count := FRegistroF800Count + RegF010.RegistroF800.Count;
  end;
end;

procedure TBloco_F.WriteRegistroF990 ;
begin
  if Assigned(RegistroF990) then
  begin
     with RegistroF990 do
     begin
       QTD_LIN_F := QTD_LIN_F + 1;
       ///
       Add( LFill('F990') +
            LFill(QTD_LIN_F,0) );
     end;
  end;
end;

(*Por: Edilson Alves de Oliveira*)
procedure TBloco_F.WriteRegistroF500(RegF010: TRegistroF010);
  var
    intFor : integer;
begin
  //(*) Os registros referentes à escrituração do PIS/Pasep e da Cofins das pessoas jurídicas sujeitas ao regime de tributação
  //com  base  no  lucro  presumido,  aplicável  para  os  fatos  geradores  a  ocorrer  a  partir  de  01  de  janeiro  de  2012,  serão
  //disponibilizados  pelo  Programa  Validador  e  Assinador  (PVA)  da  EFD-PIS/Cofins,  versão  1.05,  com  previsão  de
  //disponibilização pela Receita Federal em janeiro/2012.
  if DT_INI >= EncodeDate(2012,01,01) then
  begin
     if Assigned(RegF010.RegistroF500) then
     begin
        for intFor := 0 to RegF010.RegistroF500.Count - 1 do
        begin
           with RegF010.RegistroF500.Items[intFor] do
           begin
             Add( LFill('F500')                +
                  LFill( VL_REC_CAIXA,0,2 )    +
                  LFill( CstPisToStr(CST_PIS) )  +
                  LFill( VL_DESC_PIS ,0,2 )    +
                  DFill( VL_BC_PIS, 2 )        +
                  DFill( ALIQ_PIS, 4 )         +
                  LFill( VL_PIS,0,2 )          +
                  LFill( CstCofinsToStr(CST_COFINS) ) +
                  LFill( VL_DESC_COFINS ,0,2 ) +
                  DFill( VL_BC_COFINS, 2 )     +
                  DFill( ALIQ_COFINS, 4 )      +
                  LFill( VL_COFINS,0,2 )       +
                  LFill( COD_MOD )             +
                  LFill( CFOP, 4, True )       + //Pode ser nulo.
                  LFill( COD_CTA )             +
                  LFill( INFO_COMPL ) ) ;
           end;
           ///
           RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
        end;
        /// Variavél para armazenar a quantidade de registro do tipo.
        FRegistroF500Count := FRegistroF500Count + RegF010.RegistroF500.Count;
     end;
  end;
end;

function TBloco_F.RegistroF560New: TRegistroF560;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF560.New;
end;

function TBloco_F.RegistroF569New: TRegistroF569;
  var
    F010Count: integer;
    F560Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F560Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF560.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF560.Items[F560Count].RegistroF569.New;
end;

function TBloco_F.RegistroF510New: TRegistroF510;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF510.New;
end;

function TBloco_F.RegistroF519New: TRegistroF519;
  var
    F010Count: integer;
    F510Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   F510Count := FRegistroF001.RegistroF010.Items[F010Count].RegistroF510.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF510.Items[F510Count].RegistroF519.New;
end;

(*Por: Edilson Alves de Oliveira*)
procedure TBloco_F.WriteRegistroF510(RegF010: TRegistroF010);
  var
    intFor : integer;
begin
  //(*) Os registros referentes à escrituração do PIS/Pasep e da Cofins das pessoas jurídicas sujeitas ao regime de tributação
  //com  base  no  lucro  presumido,  aplicável  para  os  fatos  geradores  a  ocorrer  a  partir  de  01  de  janeiro  de  2012,  serão
  //disponibilizados  pelo  Programa  Validador  e  Assinador  (PVA)  da  EFD-PIS/Cofins,  versão  1.05,  com  previsão  de
  //disponibilização pela Receita Federal em janeiro/2012.
  if DT_INI >= EncodeDate(2012,01,01) then
  begin
     if Assigned(RegF010.RegistroF510) then
     begin
        for intFor := 0 to RegF010.RegistroF510.Count - 1 do
        begin
           with RegF010.RegistroF510.Items[intFor] do
           begin
             Add( LFill('F510')                  +
                  LFill( VL_REC_CAIXA,0,2 )      +
                  LFill( CstPisToStr(CST_PIS) )  +
                  LFill( VL_DESC_PIS ,0,2 )      +
                  LFill( QUANT_BC_PIS ,0,2 )     +
                  LFill( ALIQ_PIS_QUANT,0,2 )    +
                  LFill( VL_PIS,0,2 )            +
                  LFill( CstCofinsToStr(CST_COFINS) ) +
                  LFill( VL_DESC_COFINS ,0,2 )   +
                  LFill( QUANT_BC_COFINS ,0,2 )  +
                  LFill( ALIQ_COFINS_QUANT,0,2 ) +
                  LFill( VL_COFINS,0,2 )         +
                  LFill( COD_MOD )               +
                  LFill( CFOP, 4 )               +
                  LFill( COD_CTA )               +
                  LFill( INFO_COMPL ) ) ;
           end;
           ///
           RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
        end;
        /// Variavél para armazenar a quantidade de registro do tipo.
        FRegistroF510Count := FRegistroF510Count + RegF010.RegistroF510.Count;
     end;
  end;
end;
function TBloco_F.RegistroF525New: TRegistroF525;
  var
    F010Count: integer;
begin
   F010Count := FRegistroF001.RegistroF010.Count -1;
   //
   Result    := FRegistroF001.RegistroF010.Items[F010Count].RegistroF525.New;
end;

procedure TBloco_F.WriteRegistroF525(RegF010: TRegistroF010);
  var
    intFor : integer;
    strIND_REC : String;
begin
  //(*) Os registros referentes à escrituração do PIS/Pasep e da Cofins das pessoas jurídicas sujeitas ao regime de tributação
  //com  base  no  lucro  presumido,  aplicável  para  os  fatos  geradores  a  ocorrer  a  partir  de  01  de  janeiro  de  2012,  serão
  //disponibilizados  pelo  Programa  Validador  e  Assinador  (PVA)  da  EFD-PIS/Cofins,  versão  1.05,  com  previsão  de
  //disponibilização pela Receita Federal em janeiro/2012.
  if DT_INI >= EncodeDate(2012,01,01) then
  begin
     if Assigned(RegF010.RegistroF525) then
     begin
        for intFor := 0 to RegF010.RegistroF525.Count - 1 do
        begin
           with RegF010.RegistroF525.Items[intFor] do
           begin
             case IND_REC of
               crCliente         : strIND_REC:='01';
               crAdministradora  : strIND_REC:='02';
               crTituloDeCredito : strIND_REC:='03';
               crDocumentoFiscal : strIND_REC:='04';
               crItemVendido     : strIND_REC:='05';
               crOutros          : strIND_REC:='99';
             end;

             Add( LFill('F525')           +
                  LFill( VL_REC,0,2 )     +
                  LFill( strIND_REC )     +
                  LFill( CNPJ_CPF   )     +
                  LFill( NUM_DOC )     +
                  LFill( COD_ITEM   )     +
                  LFill( VL_REC_DET,0,2 ) +
                  LFill( CstPisToStr(CST_PIS) ) +
                  LFill( CstCofinsToStr(CST_COFINS)) +
                  LFill( INFO_COMPL)      +
                  LFill( COD_CTA ) ) ;
           end;
           ///
           RegistroF990.QTD_LIN_F := RegistroF990.QTD_LIN_F + 1;
        end;
        /// Variavél para armazenar a quantidade de registro do tipo.
        FRegistroF525Count := FRegistroF525Count + RegF010.RegistroF525.Count;
     end;
  end;
end;

end.
