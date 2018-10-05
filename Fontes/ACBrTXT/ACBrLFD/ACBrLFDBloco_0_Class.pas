{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Juliana Tamizou                      }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrLFDBloco_0_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrLFD3505, ACBrLFDBlocos, ACBrLFDBloco_0,
     ACBrTXTClass;


type
  /// TBLOCO_0 - Abertura, Identificação e Referências

  { TBloco_0 }

  TBloco_0 = class(TACBrLFD3505)
  private
    FOnBeforeWriteRegistro0200: TWriteRegistroEvent;
    FOnBeforeWriteRegistro0210: TWriteRegistroEvent;
    FOnBeforeWriteRegistro0990: TWriteRegistroEvent;

    FOnWriteRegistro0200: TWriteRegistroEvent;
    FOnWriteRegistro0210: TWriteRegistroEvent;
    FOnWriteRegistro0990: TWriteRegistroEvent;

    FOnAfterWriteRegistro0200: TWriteRegistroEvent;
    FOnAfterWriteRegistro0210: TWriteRegistroEvent;
    FOnAfterWriteRegistro0990: TWriteRegistroEvent;

    FRegistro0000: TRegistro0000;      /// BLOCO 0 - Registro0000
    FRegistro0001: TRegistro0001;      /// BLOCO 0 - Registro0001
    FRegistro0990: TRegistro0990;      /// BLOCO 0 - Registro0990

    FRegistro0005Count: Integer;
    FRegistro0025Count: Integer;
    FRegistro0030Count: Integer;
    FRegistro0100Count: Integer;
    FRegistro0125Count: Integer;
    FRegistro0120Count: Integer;
    FRegistro0150Count: Integer;
    FRegistro0175Count: Integer;
    FRegistro0200Count: Integer;
    FRegistro0205Count: Integer;
    FRegistro0210Count: Integer;
    FRegistro0215Count: Integer;
    FRegistro0400Count: Integer;
    FRegistro0450Count: Integer;
    FRegistro0455Count: Integer;
    FRegistro0460Count: Integer;
    FRegistro0465Count: Integer;
    FRegistro0470Count: Integer;

    procedure WriteRegistro0005(Reg0001: TRegistro0001);
    procedure WriteRegistro0025(Reg0001: TRegistro0001);
    procedure WriteRegistro0030(Reg0001: TRegistro0001);
    procedure WriteRegistro0035(Reg0030: TRegistro0030);
    procedure WriteRegistro0040(Reg0030: TRegistro0030);
    procedure WriteRegistro0045(Reg0030: TRegistro0030);
    procedure WriteRegistro0100(Reg0001: TRegistro0001);
    procedure WriteRegistro0125(Reg0001: TRegistro0001);
    procedure WriteRegistro0120(Reg0001: TRegistro0001);
    procedure WriteRegistro0150(Reg0001: TRegistro0001);
    procedure WriteRegistro0175(Reg0150: TRegistro0150);
    procedure WriteRegistro0200(Reg0001: TRegistro0001);
    procedure WriteRegistro0205(Reg0200: TRegistro0200);
    procedure WriteRegistro0210(Reg0200: TRegistro0200);
    procedure WriteRegistro0215(Reg0200: TRegistro0200);
    procedure WriteRegistro0400(Reg0001: TRegistro0001);
    procedure WriteRegistro0450(Reg0001: TRegistro0001);
    procedure WriteRegistro0455(Reg0450: TRegistro0450);
    procedure WriteRegistro0460(Reg0450: TRegistro0450);
    procedure WriteRegistro0465(Reg0450: TRegistro0450);
    procedure WriteRegistro0470(Reg0450: TRegistro0450);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    function Registro0000New: TRegistro0000;
    function Registro0001New: TRegistro0001;
    function Registro0005New: TRegistro0005;
    function Registro0025New: TRegistro0025;
    function Registro0030New: TRegistro0005;
    function Registro0035New: TRegistro0005;
    function Registro0100New: TRegistro0100;
    function Registro0125New: TRegistro0125;
    function Registro0150New: TRegistro0150;
    function Registro0175New: TRegistro0175;
    function Registro0200New: TRegistro0200;
    function Registro0205New: TRegistro0205;
    function Registro0210New: TRegistro0210;
    function Registro0215New: TRegistro0215;
    function Registro0400New: TRegistro0400;
    function Registro0450New: TRegistro0450;
    function Registro0460New: TRegistro0460;

    procedure WriteRegistro0000;
    procedure WriteRegistro0001;
    procedure WriteRegistro0990;

    property Registro0000: TRegistro0000 read FRegistro0000 write FRegistro0000;
    property Registro0001: TRegistro0001 read FRegistro0001 write FRegistro0001;
    property Registro0990: TRegistro0990 read FRegistro0990 write FRegistro0990;

    property Registro0005Count: Integer read FRegistro0005Count write FRegistro0005Count;
    property Registro0025Count: Integer read FRegistro0025Count write FRegistro0025Count;
    property Registro0030Count: Integer read FRegistro0030Count write FRegistro0030Count;
    property Registro0120Count: Integer read FRegistro0120Count write FRegistro0120Count;
    property Registro0150Count: Integer read FRegistro0150Count write FRegistro0150Count;
    property Registro0175Count: Integer read FRegistro0175Count write FRegistro0175Count;
    property Registro0200Count: Integer read FRegistro0200Count write FRegistro0200Count;
    property Registro0205Count: Integer read FRegistro0205Count write FRegistro0205Count;
    property Registro0400Count: Integer read FRegistro0400Count write FRegistro0400Count;
    property Registro0450Count: Integer read FRegistro0450Count write FRegistro0450Count;
    property Registro0460Count: Integer read FRegistro0460Count write FRegistro0460Count;
    property Registro0465Count: Integer read FRegistro0465Count write FRegistro0465Count;

    property OnBeforeWriteRegistro0200: TWriteRegistroEvent read FOnBeforeWriteRegistro0200 write FOnBeforeWriteRegistro0200;
    property OnBeforeWriteRegistro0210: TWriteRegistroEvent read FOnBeforeWriteRegistro0210 write FOnBeforeWriteRegistro0210;
    property OnBeforeWriteRegistro0990: TWriteRegistroEvent read FOnBeforeWriteRegistro0990 write FOnBeforeWriteRegistro0990;

    property OnWriteRegistro0200      : TWriteRegistroEvent read FOnWriteRegistro0200       write FOnWriteRegistro0200;
    property OnWriteRegistro0210      : TWriteRegistroEvent read FOnWriteRegistro0210       write FOnWriteRegistro0210;
    property OnWriteRegistro0990      : TWriteRegistroEvent read FOnWriteRegistro0990       write FOnWriteRegistro0990;

    property OnAfterWriteRegistro0200 : TWriteRegistroEvent read FOnAfterWriteRegistro0200  write FOnAfterWriteRegistro0200;
    property OnAfterWriteRegistro0210 : TWriteRegistroEvent read FOnAfterWriteRegistro0210  write FOnAfterWriteRegistro0210;
    property OnAfterWriteRegistro0990 : TWriteRegistroEvent read FOnAfterWriteRegistro0990  write FOnAfterWriteRegistro0990;
  end;

implementation

uses StrUtils,
     ACBrTXTUtils;

{ TBloco_0 }

constructor TBloco_0.Create ;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_0.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_0.CriaRegistros;
begin
  FRegistro0000 := TRegistro0000.Create;
  FRegistro0001 := TRegistro0001.Create;
  FRegistro0990 := TRegistro0990.Create;

  FRegistro0005Count := 0;
  FRegistro0025Count := 0;
  FRegistro0030Count := 0;
  FRegistro0175Count := 0;
  FRegistro0120Count := 0;
  FRegistro0150Count := 0;
  FRegistro0200Count := 0;
  FRegistro0400Count := 0;
  FRegistro0450Count := 0;
  FRegistro0465Count := 0;

  FRegistro0990.QTD_LIN_0 := 0;
end;

procedure TBloco_0.LiberaRegistros;
begin
  FRegistro0000.Free;
  FRegistro0001.Free;
  FRegistro0990.Free;
end;

procedure TBloco_0.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_0.Registro0000New: TRegistro0000;
begin
   Result := FRegistro0000;
end;

function TBloco_0.Registro0001New: TRegistro0001;
begin
   Result := FRegistro0001;
end;

function TBloco_0.Registro0005New: TRegistro0005;
begin
   Result := FRegistro0001.Registro0005;
end;

function TBloco_0.Registro0025New: TRegistro0025;
begin
   Result := FRegistro0001.Registro0025.New(FRegistro0001);
end;

function TBloco_0.Registro0030New: TRegistro0005;
begin

end;

function TBloco_0.Registro0035New: TRegistro0005;
begin

end;

function TBloco_0.Registro0100New: TRegistro0100;
begin
   Result := FRegistro0001.Registro0100;
end;

function TBloco_0.Registro0125New: TRegistro0125;
begin
   Result := FRegistro0001.Registro0125;
end;

function TBloco_0.Registro0150New: TRegistro0150;
begin
   Result := FRegistro0001.Registro0150.New(FRegistro0001);
end;

function TBloco_0.Registro0175New: TRegistro0175;
var
U0150: TRegistro0150;
U0150Count: Integer;
begin
   U0150Count := FRegistro0001.Registro0150.Count -1;
   if U0150Count = -1 then
      raise Exception.Create('O registro 0175 deve ser filho do registro 0150, e não existe nenhum 0150 pai!');

   U0150 := FRegistro0001.Registro0150.Items[U0150Count];
   Result  := U0150.Registro0175.New(U0150);
end;

function TBloco_0.Registro0200New: TRegistro0200;
begin
   Result := FRegistro0001.Registro0200.New(FRegistro0001);
end;

function TBloco_0.Registro0205New: TRegistro0205;
var
U0200: TRegistro0200;
U0200Count: Integer;
begin
   U0200Count := FRegistro0001.Registro0200.Count -1;
   if U0200Count = -1 then
      raise Exception.Create('O registro 0205 deve ser filho do registro 0200, e não existe nenhum 0200 pai!');

   U0200 := FRegistro0001.Registro0200.Items[U0200Count];
   Result  := U0200.Registro0205.New(U0200);
end;

function TBloco_0.Registro0210New: TRegistro0210;
var
U0200: TRegistro0200;
U0200Count: Integer;
begin
   U0200Count := FRegistro0001.Registro0200.Count -1;
   if U0200Count = -1 then
      raise Exception.Create('O registro 0210 deve ser filho do registro 0200, e não existe nenhum 0200 pai!');

   U0200 := FRegistro0001.Registro0200.Items[U0200Count];
end;

function TBloco_0.Registro0215New: TRegistro0215;
var
U0200: TRegistro0200;
U0200Count: Integer;
begin
   U0200Count := FRegistro0001.Registro0200.Count -1;
   if U0200Count = -1 then
      raise Exception.Create('O registro 0215 deve ser filho do registro 0200, e não existe nenhum 0200 pai!');

   U0200 := FRegistro0001.Registro0200.Items[U0200Count];
end;


function TBloco_0.Registro0400New: TRegistro0400;
begin
   Result := FRegistro0001.Registro0400.New(FRegistro0001);
end;

function TBloco_0.Registro0450New: TRegistro0450;
begin
   Result := FRegistro0001.Registro0450.New(FRegistro0001);
end;

function TBloco_0.Registro0460New: TRegistro0460;
begin
   //Result := FRegistro0001.Registro0460.New(FRegistro0450);
end;

procedure TBloco_0.WriteRegistro0000 ;
var
  strIND_PERFIL: AnsiString;
  strCOD_VER,strCOD_FIN: AnsiString;
begin
  if Assigned(Registro0000) then
  begin
     with Registro0000 do
     begin
       case COD_VER of
         vlVersao1001: strCOD_VER := '001';
         vlVersao1002: strCOD_VER := '002';
         vlVersao1003: strCOD_VER := '003';
         vlVersao1004: strCOD_VER := '1004';
         vlVersao1005: strCOD_VER := '1005';
         vlVersao2000: strCOD_VER := '2000';
       end;

       case COD_FIN of
         ralRegular           : strCOD_FIN := '00';
         ralSubstituto        : strCOD_FIN := '01';
         ralDadosAdicionais   : strCOD_FIN := '02';
         ralIntimacaoEsp      : strCOD_FIN := '03';
         ralCorrecaoIDP       : strCOD_FIN := '04';
         ralPubDiarioOficial  : strCOD_FIN := '05';
         ralSintegraRegular   : strCOD_FIN := '15';
         ralSintegraSubstituto: strCOD_FIN := '16';

         ralSintegraDadosAdicionais : strCOD_FIN := '17';
         ralSintegraRegularICMSST   : strCOD_FIN := '18';
         ralSintegraSubstitutoICMSST: strCOD_FIN := '19';
         ralSintegraDadosAdicionaisICMSST: strCOD_FIN := '20';

         ralRegularSefin        : strCOD_FIN := '25';
         ralSubstitutoSefin     : strCOD_FIN := '26';
         ralDadosAdicionaisSefin: strCOD_FIN := '27';
         ralEmissaoDocumento    : strCOD_FIN := '30';
         ralEmissaoDocAvulso    : strCOD_FIN := '31';
         ralSolicAuditorFical   : strCOD_FIN := '61';
         ralEntregaSecretariaReceita: strCOD_FIN := '62';
         ralInfComplementarSefaz: strCOD_FIN := '90';
       end;

       Check(funChecaCNPJ(CNPJ), '(0-0000) ENTIDADE: O CNPJ "%s" digitado é inválido!', [CNPJ]);
       Check(funChecaUF(UF), '(0-0000) ENTIDADE: A UF "%s" digitada é inválido!', [UF]);
       Check(funChecaIE(IE, UF), '(0-0000) ENTIDADE: A inscrição estadual "%s" digitada é inválida!', [IE]);
       //Check(funChecaMUN(COD_MUN), '(0-0000) ENTIDADE: O código do município "%s" digitado é inválido!', [IntToStr(COD_MUN)]);

       Add( LFill( '0000' )     +
            LFill( 'LFPD' )     +
            LFill( strCOD_VER ) + //70/05
            LFill( 0, 1)        + //70/05
            LFill( strCOD_FIN ) + //70/05
            LFill( DT_INI )     +
            LFill( DT_FIN )     +
            LFill( 2, 1)        + //70/05
            LFill( 0, 1)        + //70/05
            LFill( NOME )       +
            LFill( CNPJ )       +
            LFill('')           + //70/05
            LFill('0')           + //70/05
            LFill( UF )         +
            LFill( IE )         +
            LFill( COD_MUN, 7 ) +
            LFill( IM )         +
            //LFill( 0, 1, true )   +
            LFill( SUFRAMA ));//    +
            //LFill( strCOD_VER ) +
            //LFill( strCOD_FIN ) +
            //LFill( Integer(COD_CONTEUDO), 2 ) +
            //LFill( 'Brasil' )   +
            //LFill( FANTASIA )   +
            //LFill( NIRE, 11 )   +
            //LFill( 0, 1, true ) +
            {LFill( 0, 1, true ) ); }

       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0001 ;
begin
  if Assigned(FRegistro0001) then
  begin
     with FRegistro0001 do
     begin
        Add( LFill( '0001' ) +
             LFill( Integer(IND_MOV), 0 ) ) ;

        if IND_MOV = imlComDados then
        begin
          WriteRegistro0005(FRegistro0001) ;
          //WriteRegistro0025(FRegistro0001) ;
          //WriteRegistro0030(FRegistro0001) ;
          WriteRegistro0100(FRegistro0001) ;
          WriteRegistro0125(FRegistro0001) ;
          //WriteRegistro0120(FRegistro0001) ;
          WriteRegistro0150(FRegistro0001) ;
          WriteRegistro0200(FRegistro0001) ;
          WriteRegistro0400(FRegistro0001) ;
          WriteRegistro0450(FRegistro0001) ;
        end;
     end;

     Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
  end;
end;

procedure TBloco_0.WriteRegistro0005(Reg0001: TRegistro0001) ;
begin
  if Assigned(Reg0001.Registro0005) then
  begin
     with Reg0001.Registro0005 do
     begin
       Check(funChecaCPF(CPFRESP), '(0-0005) COMPLEMENTO DO CONTRIBUINTE: %s, o CPF "%s" digitado é inválido!', [NOMERESP, CPFRESP]);
       Check(funChecaCEP(CEP, Registro0000.UF), '(0-0005) COMPLEMENTO DO CONTRIBUINTE "%s": O CEP "%s" digitada é inválido para a unidade de federação "%s"!', [NOMERESP, CEP, Registro0000.UF]);
       ///
       Add( LFill('0005')     +
            LFill(NOMERESP)   +  // 70/05
            LFill(CEP, 8)     +  // 70/05
            LFill(ENDERECO)   +  // 70/05
            LFill(NUM)        +  // 70/05
            LFill(COMPL)      +  // 70/05
            LFill(BAIRRO)     +  // 70/05
            LFill(CEP_CP, 8)  +  // 70/05
            LFill(CP)         +  // 70/05
            LFill(FONE)       +  // 70/05
            LFill(FAX)        +  // 70/05
            LFill(EMAIL) ) ;     // 70/05
            //LFill(NOMERESP)   +
            //LFill(COD_ASS, 1) +
            //LFill(CPFRESP)    +
            {LFill(CEP, 8)     +
            LFill(ENDERECO)   +
            LFill(NUM)        +
            LFill(COMPL)      +
            LFill(BAIRRO)     +
            LFill(CEP_CP, 8)  +
            LFill(CP)         +
            LFill(FONE)       +
            LFill(FAX)        +
            LFill(EMAIL) ) ; }
       ///
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0005Count := FRegistro0005Count + 1;
  end;
end;

procedure TBloco_0.WriteRegistro0025(Reg0001: TRegistro0001);
var
  intFor: Integer;
  strCODBF_ICMS: AnsiString;
  strCODBF_ISS: AnsiString;
begin
   for intFor := 0 to Reg0001.Registro0025.Count - 1 do
   begin
      with Reg0001.Registro0025.Items[intFor] do
      begin
         //Check((Integer(CODBF_ICMS) > 1) and (Registro0000.UF <> 'DF'),
         //      '(0-0025) BENEFICIO FISCAL, o Código do Benefício Fiscal de ICMS "%s"  é inválido para esta UF %s!', [CODBF_ICMS, Registro0000.UF] );

         //Check((Integer(CODBF_ICMS) = 1) and (Registro0000.UF <> 'DF'),
         //      '(0-0025) BENEFICIO FISCAL, o Código do Benefício Fiscal de ICMS "%s"  é inválido para esta UF %s!', [CODBF_ICMS, Registro0000.UF] );

         case CODBF_ICMS of
           bDF001: strCODBF_ICMS := 'DF001';
           bPE001: strCODBF_ICMS := 'PE001';
           bPE002: strCODBF_ICMS := 'PE002';
           bPE003: strCODBF_ICMS := 'PE003';
           bPE004: strCODBF_ICMS := 'PE004';
           bPE005: strCODBF_ICMS := 'PE005';
         else
           strCODBF_ICMS:= '';
         end;

         if CODBF_ISS  = bSDF001 then
            strCODBF_ISS := 'DF001'
         else
            strCODBF_ISS:= '';

         Add( LFill('0025') +
              LFill(strCODBF_ICMS) +
              LFill(strCODBF_ISS) );
      end;

      Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
   end;
   // Variavél para armazenar a quantidade de registro do tipo.
   FRegistro0025Count := FRegistro0025Count + Reg0001.Registro0025.Count;
end;

procedure TBloco_0.WriteRegistro0030(Reg0001: TRegistro0001);
var
  strIND_ISS, strIND_ICMS: Char;
  strIND_IPI, strIND_ST: Char;
  strIND_RI,strIND_RIDF: Char;
  strIND_RUDF,strIND_LMC: Char;
  strIND_LCV: Char;
begin
  if Assigned(Reg0001.Registro0030) then
  begin
     with Reg0001.Registro0030 do
     begin
       strIND_ISS  := IfThen(IND_ISS, '1', '2' );
       strIND_ICMS := IfThen(IND_ICMS, '1', '2' );
       strIND_IPI  := IfThen(IND_IPI, '1', '2' );
       strIND_ST   := IfThen(IND_IST, '1', '2' );
       strIND_RI   := IfThen(IND_RI, '1', '2' );
       strIND_RIDF := IfThen(IND_RIDF, '1', '2' );
       strIND_RUDF := IfThen(IND_RUDF, '1', '2' );
       strIND_LMC  := IfThen(IND_LMC, '1', '2' );
       strIND_LCV  := IfThen(IND_LCV, '1', '2' );

       Add( LFill('0030') +
            LFill(Integer(IND_ENTDADOS), 1) +
            LFill(Integer(IND_CONTARQ), 1)  +
            LFill(strIND_ISS)  +
            LFill(strIND_ICMS) +
            LFill(strIND_IPI)  +
            LFill(strIND_ST)   +
            LFill(Integer(IND_TIPOESCR_FISCAL),1 ) +
            LFill(Integer(IND_TIPOESCR_CONT),1 )   +
            LFill(strIND_RI)   +
            LFill(strIND_RIDF) +
            LFill(strIND_RUDF) +
            LFill(strIND_LMC)  +
            LFill(strIND_LCV) ) ;
       ///

       /// Registros FILHOS
       WriteRegistro0035( Reg0001.Registro0030 );
       WriteRegistro0040( Reg0001.Registro0030 );
       WriteRegistro0045( Reg0001.Registro0030 );

       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0035(Reg0030: TRegistro0030);
begin
   if Assigned(Reg0030.Registro0035) then
   begin
      with Reg0030.Registro0035 do
      begin
         Add( LFill('0035') +
              LFill(Integer(IND_RT), 1)    +
              LFill(Integer(ANEXO_RT), 1)  +
              LFill(ALIQ_RT)  +
              LFill(DTINI_RT) +
              LFill(DTFIM_RT) +
              LFill('DOCUMENTO EMITIDO POR ME/EPP OPTANTE PELO SIMPLES NACIONAL')  +
              LFill('ISS retido pelo tomador, nos termos do Art. 21 DA LC 123/06, '+
                    'correspondente à aplicação da alíquota de %ALIQ_RT% % sobre a base de cálculo destacada.') );

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
   end;
end;

procedure TBloco_0.WriteRegistro0040(Reg0030: TRegistro0030);
begin
   if Assigned(Reg0030.Registro0040) then
   begin
      with Reg0030.Registro0040 do
      begin
         Add( LFill('0040') +
              LFill(Integer(IND_CRD), 1)    +
              LFill(Integer(ANEXO_CRD), 1)  +
              LFill(ALIQ_CRD)  +
              LFill(DTINI_CRD) +
              LFill(DTFIM_CRD) +
              LFill('DOCUMENTO EMITIDO POR ME/EPP OPTANTE PELO SIMPLES NACIONAL')  +
              LFill('Não gera direito a crédito fiscal, de acordo com o § 4º do art. 23 da LC 123/06') +
              LFill('Não gera direito a crédito fiscal de ISS e IPI') +
              LFill('Nos termos do art. 23 da LC 123/06 é permitido o aproveitamento do crédito '+
                    'de ICMS destacado neste documento, correspondente à aplicação da alíquota de '+
                    '%ALIQ_CRD% % sobre a base de cálculo destacada'));

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
   end;
end;

procedure TBloco_0.WriteRegistro0045(Reg0030: TRegistro0030);
begin
   if Assigned(Reg0030.Registro0045) then
   begin
      with Reg0030.Registro0045 do
      begin
         Add( LFill('0045') +
              LFill(ALIQ_STS)  +
              LFill(DTINI_STS) +
              LFill(DTFIM_STS) +
              LFill('DOCUMENTO EMITIDO POR ME/EPP OPTANTE PELO SIMPLES NACIONAL')     +
              LFill('ICMS-ST retido pela ME/EPP, responsável pelo imposto devido '    +
                    'pelo adquirente, relativo à aplicação da alíquota interna sobre '+
                    'a base de cálculo específica, com a dedução permitida sobre a '  +
                    'operação própria, conforme §§ 9º e 10 do art 3º da Res CGSN 51/08'));

         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
   end;
end;

procedure TBloco_0.WriteRegistro0100(Reg0001: TRegistro0001) ;
begin
  if Assigned(Reg0001.Registro0100) then
  begin
     with Reg0001.Registro0100 do
     begin
       Check(funChecaCPF(CPF),     '(0-0100) CONTADOR: %s, o CPF "%s" digitado é inválido!', [NOME, CPF]);
       Check(funChecaCNPJ(CNPJ),   '(0-0100) CONTADOR: %s, o CNPJ "%s" digitado é inválido!', [NOME, CNPJ]);
//       Check(funChecaCEP(CEP, Registro0000.UF), '(0-0100) CONTADOR: %s, o CEP "%s" digitada é inválido para a unidade de federação "%s"!', [NOME, CEP, Registro0000.UF]);
       Check(NOME <> '', '(0-0100) CONTADOR: O nome do contabilista/escritório é obrigatório!');
       ///
       Add( LFill('0100')   +
            LFill(NOME)     +
            LFill(CNPJ)     + // 70/05
            LFill(CPF)      + // 70/05
            LFill(CRC)      + // 70/05
            LFill(UF)       + // 70/05
            LFill(CEP, 8)   + // 70/05
            LFill(ENDERECO) + // 70/05
            LFill(NUM)      + // 70/05
            LFill(COMPL)    + // 70/05
            LFill(BAIRRO)   + // 70/05
            LFill(CEP_CF,8) + // 70/05
            LFill(CP)       + // 70/05
            LFill(FONE)     + // 70/05
            LFill(FAX)      + // 70/05
            LFill(EMAIL)) ; // 70/05
            {LFill(COD_ASS)  +
            LFill(CNPJ)     +
            LFill(CPF)      +
            LFill(CRC)      +
            LFill(UF)       +
            LFill(CEP, 8)   +
            LFill(ENDERECO) +
            LFill(NUM)      +
            LFill(COMPL)    +
            LFill(BAIRRO)   +
            LFill(CEP_CF,8) +
            LFill(CP)       +
            LFill(FONE)     +
            LFill(FAX)      +
            LFill(EMAIL)) ;  }
       ///
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0125(Reg0001: TRegistro0001);
begin
  if Assigned(Reg0001.Registro0125) then
  begin
     with Reg0001.Registro0125 do
     begin
       Check(funChecaCPF(CPF),     '(0-0125) CONTADOR: %s, o CPF "%s" digitado é inválido!', [NOME, CPF]);
       Check(funChecaCNPJ(CNPJ),   '(0-0125) CONTADOR: %s, o CNPJ "%s" digitado é inválido!', [NOME, CNPJ]);
//       Check(funChecaCEP(CEP, Registro0000.UF), '(0-0125) CONTADOR: %s, o CEP "%s" digitada é inválido para a unidade de federação "%s"!', [NOME, CEP, Registro0000.UF]);
       Check(NOME <> '', '(0-0125) CONTADOR: O nome do contabilista/escritório é obrigatório!');
       ///
       Add( LFill('0125')   +
            LFill(NOME)     +
            LFill(CNPJ)     + // 70/05
            LFill(CPF)      + // 70/05
            LFill(FONE)     + // 70/05
            LFill(FAX)      + // 70/05
            LFill(EMAIL)) ; // 70/05
       ///
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0120(Reg0001: TRegistro0001);
var
  booExterior: Boolean;
begin
   if Assigned(Reg0001.Registro0120) then
   begin
      with Reg0001.Registro0120 do
      begin
         Check(funChecaCNPJ(CNPJ),   '(0-0120) EMITENTE AVULSO: o CNPJ "%s" digitado é inválido!', [CNPJ]);


         Check(funChecaCPF(CPF),     '(0-0100) EMITENTE AVULSO: , o CPF "%s" digitado é inválido!', [CPF]);

         //Check(funChecaMUN(COD_MUN), '(0-0100) EMITENTE AVULSO: %s, o código do município "%s" digitado é inválido!', [IntToStr(COD_MUN)]);
       ///
       Add( LFill('0120')     +
            LFill(CNPJ)       +
            LFill(FISCO)      +
            LFill(UF)         +
            LFill(COD_MUN, 7) +
            LFill(COD_SETOR)  +
            LFill(CPF)        +
            LFill(MATRICULA) );
       ///
       Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
  end;
end;

procedure TBloco_0.WriteRegistro0150(Reg0001: TRegistro0001) ;
var
  intFor: integer;
  booExterior: Boolean;
begin
  if Assigned(Reg0001.Registro0150) then
  begin
     for intFor := 0 to Reg0001.Registro0150.Count - 1 do
     begin
        with Reg0001.Registro0150.Items[intFor] do
        begin
           booExterior := (StrToIntDef(COD_PAIS,0) <>  1058);

          if Length(CNPJ) > 0 then
             Check(funChecaCNPJ(CNPJ), '(0-0150) %s-%s, o CNPJ "%s" digitado é inválido!', [COD_PART, NOME, CNPJ]);
          if Length(CPF)  > 0 then
             Check(funChecaCPF(CPF),   '(0-0150) %s-%s, o CPF "%s" digitado é inválido!', [COD_PART, NOME, CPF]);
          Check(NOME <> '',            '(0-0150) O nome do participante com CPF/CNPJ %s é obrigatório!', [CNPJ + CPF]);
          Check(ENDERECO <> EmptyStr,  '(0-150) O Endereço do participante "%s - %s - CPF/CNPJ %s" é obrigatório!', [COD_PART, NOME, CNPJ + CPF]);
          ///
          Add( LFill('0150')   +
               LFill(COD_PART) +
               LFill(NOME)     +
               LFill(COD_PAIS) +
               LFill(CNPJ)     +
               LFill(CPF)      +
               LFill('')       +
               LFill('')       +
               LFill(UF)       +
               LFill(IE)       +
               LFill(IE_ST)    +
               IfThen(booExterior, LFill(''), LFill(COD_MUN, 7)) +
               LFill(IM)    +
               LFill(SUFRAMA) );
        end;
        /// Registros FILHOS
        WriteRegistro0175( Reg0001.Registro0150.Items[intFor] );

        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0150Count := FRegistro0150Count + Reg0001.Registro0150.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0175(Reg0150: TRegistro0150) ;
var
  intFor: integer;
begin
  if Assigned(Reg0150.Registro0175) then
  begin
     for intFor := 0 to Reg0150.Registro0175.Count - 1 do
     begin
        with Reg0150.Registro0175.Items[intFor] do
        begin
           Add( LFill('0175')   +
                LFill(CEP,8)    +
                LFill(ENDERECO) +
                LFill(NUM)      +
                LFill(COMPL)    +
                LFill(BAIRRO)   +
                LFill(CEP_CP,8) +
                LFill(CP)       +
                LFill(FONE)     +
                LFill(FAX) );

        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0175Count := FRegistro0175Count + Reg0150.Registro0175.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0200(Reg0001: TRegistro0001) ;
var
  intFor: integer;
  strTIPO_ITEM: AnsiString;
  strLinha: AnsiString;
begin
  if Assigned( Reg0001.Registro0200 ) then
  begin
     //-- Before
     strLinha := '';
     if Assigned(FOnBeforeWriteRegistro0200) then
     begin
        FOnBeforeWriteRegistro0200(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to Reg0001.Registro0200.Count - 1 do
     begin
        strLinha := '';
        with Reg0001.Registro0200.Items[intFor] do
        begin
           {if COD_GEN <> EmptyStr then
           begin
              COD_GEN := funStrZero(COD_GEN, 2);
              Check(funChecaGENERO(COD_GEN), '(0-0200) O código do gênero "%s" digitado é inválido! ' +
                    'Produto %s %s', [COD_GEN, DESCR_ITEM]);
           end;}

           strLinha := LFill('0200')       +
                       LFill( COD_ITEM )   +
                       LFill( DESCR_ITEM ) +
                       LFill( COD_GEN, 2 ) +
                       LFill( COD_LST );
           //-- Write
           if Assigned(FOnWriteRegistro0200) then
              FOnWriteRegistro0200(strLinha);

           Add(strLinha);
        end;
        /// Registros FILHOS
        WriteRegistro0205( Reg0001.Registro0200.Items[intFor] ) ;
        WriteRegistro0210( Reg0001.Registro0200.Items[intFor] ) ;
        WriteRegistro0215( Reg0001.Registro0200.Items[intFor] );

        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(FOnAfterWriteRegistro0200) then
     begin
        FOnAfterWriteRegistro0200(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variável para armazenar a quantidade de registro do tipo.
     FRegistro0200Count := FRegistro0200Count + Reg0001.Registro0200.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0205(Reg0200: TRegistro0200) ;
var
  intFor: integer;
begin
  if Assigned( Reg0200.Registro0205 ) then
  begin
     for intFor := 0 to Reg0200.Registro0205.Count - 1 do
     begin
        with Reg0200.Registro0205.Items[intFor] do
        begin
          Add( LFill('0205')           +
               LFill( COD_ANT_ITEM )   +
               LFill( DESCR_ANT_ITEM ) +
               LFill( DT_FIN )         +
               LFill( DT_FIN) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0205Count := FRegistro0205Count + Reg0200.Registro0205.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0210(Reg0200: TRegistro0200) ;
var
  intFor: Integer;
begin
   if Assigned( Reg0200.Registro0210 ) then
   begin
      for intFor := 0 to Reg0200.Registro0210.Count - 1 do
      begin
         with Reg0200.Registro0210.Items[intFor] do
         begin
            Add( LFill('0210')            +
                 LFill( COD_ITEM_COMP )   +
                 LFill( UNID_ITEM )       +
                 LFill( QTD_COMP )        +
                 LFill( UNID_COMP )       +
                 LFill( DT_INI_COMP )     +
                 LFill( Integer(IND_ALT)) +
                 LFill( PERDA_COMP) ) ;
         end;
         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro0210Count := FRegistro0210Count + Reg0200.Registro0210.Count;
   end;
end;

procedure TBloco_0.WriteRegistro0215(Reg0200: TRegistro0200);
var
  intFor: Integer;
begin
  if Assigned(Reg0200.Registro0215) then
   begin
      for intFor := 0 to Reg0200.Registro0215.Count - 1 do
      begin
         with Reg0200.Registro0215.Items[intFor] do
         begin
            Add(LFill('0215')      +
                LFill(CODITEM_ANP) +
                LFill(DT_INI) );
         end;
         Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
      end;
      /// Variavél para armazenar a quantidade de registro do tipo.
      FRegistro0215Count := FRegistro0215Count + Reg0200.Registro0215.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0400(Reg0001: TRegistro0001) ;
var
  intFor: integer;
begin
  if Assigned(Reg0001.Registro0400) then
  begin
     for intFor := 0 to Reg0001.Registro0400.Count - 1 do
     begin
        with Reg0001.Registro0400.Items[intFor] do
        begin
          ///
          Add( LFill('0400') +
               LFill( COD_NAT ) +
               LFill( DESCR_NAT ) ) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0400Count := FRegistro0400Count + Reg0001.Registro0400.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0450(Reg0001: TRegistro0001) ;
var
  intFor: integer;
begin
  if Assigned( Reg0001.Registro0450 ) then
  begin
     for intFor := 0 to Reg0001.Registro0450.Count - 1 do
     begin
        with Reg0001.Registro0450.Items[intFor] do
        begin
          Add( LFill('0450') +
               LFill( COD_INF ) +
               LFill( TXT ) ) ;
          WriteRegistro0465(Reg0001.Registro0450.Items[intFor]);
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0450Count := FRegistro0450Count + Reg0001.Registro0450.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0455(Reg0450: TRegistro0450);
var
  intFor: Integer;
begin
   if Assigned( Reg0450.Registro0455 ) then
   begin
     for intFor := 0 to Reg0450.Registro0455.Count - 1 do
     begin
        with Reg0450.Registro0455.Items[intFor] do
        begin
          Add( LFill('0455') +
               LFill(NORMA ) );
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0455Count := FRegistro0455Count + Reg0450.Registro0455.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0460(Reg0450: TRegistro0450);
var
  intFor: integer;
begin
  if Assigned( Reg0450.Registro0460 ) then
  begin
     for intFor := 0 to Reg0450.Registro0460.Count - 1 do
     begin
        with Reg0450.Registro0460.Items[intFor] do
        begin
          Add( LFill('0460')   +
               LFill(COD_DA)   +
               LFill(NUM_DA)   +
               LFill(VALOR_DA) +
               LFill(DT_INI)   +
               LFill(DT_FIM)   +
               LFill(DT_VENC)  +
               LFill(DT_PAGTO)) ;
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0460Count := FRegistro0460Count + Reg0450.Registro0460.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0465(Reg0450: TRegistro0450);
var
  intFor: Integer;
begin
  if Assigned( Reg0450.Registro0465 ) then
  begin
     for intFor := 0 to Reg0450.Registro0465.Count - 1 do
     begin
        with Reg0450.Registro0465.Items[intFor] do
        begin
           Check(funChecaCNPJ(CNPJ), '(0-0465) , o CNPJ do Participante "%s" digitado é inválido!', [CNPJ]);
           Check(funChecaCPF(CPF), '(0-0465) , o CPF do Participante "%s" digitado é inválido!', [CPF]);

           Add( LFill('0465')   +
                LFill(Integer(IND_OPER), 1) +
                LFill(Integer(IND_EMIT), 1) +
                LFill(CNPJ)  +
                LFill(CPF)   +
                LFill(UF)    +
                LFill(IE)    +
                LFill(CODMUN, 7) +
                LFill(IM)        +
                LFill(COD_MOD)   +
                LFill(COD_SIT)   +
                LFill(SERIE)     +
                LFill(SUBSERIE)  +
                LFill(NUMDOCTO)  +
                LFill(DT_EMISSAO)+
                LFill(VALOR_DOC,0,2, true) +
                LFill(VALOR_ISS,0,2, true) +
                LFill(VALOR_RT,0,2, true)  +
                LFill(VALOR_ICMS,0,2, true)+
                LFill(VALOR_ST,0,2, true)  +
                LFill(VALOR_IPI,0,2,true)
                );
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0465Count := FRegistro0465Count + Reg0450.Registro0465.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0470(Reg0450: TRegistro0450);
var
  intFor: Integer;
begin
   if Assigned( Reg0450.Registro0470 ) then
   begin
      for intFor := 0 to Reg0450.Registro0470.Count - 1 do
      begin
        with Reg0450.Registro0470.Items[intFor] do
        begin
           Add( LFill('0470')    +
                LFill(COD_MOD)   +
                LFill(ECF_FAB)   +
                LFill(ECF_CX)    +
                LFill(ECF_CRO)   +
                LFill(ECF_CRZ)   +
                LFill(NUMDOCTO)  +
                LFill(VALOR_DOC) +
                LFill(VALOR_ISS) +
                LFill(VALOR_ICMS) );
        end;
        Registro0990.QTD_LIN_0 := Registro0990.QTD_LIN_0 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro0470Count := FRegistro0470Count + Reg0450.Registro0470.Count;
  end;
end;

procedure TBloco_0.WriteRegistro0990 ;
  var strLinha : AnsiString;
begin
  //--Before
  strLinha := '';
  if Assigned(FOnBeforeWriteRegistro0990) then
  begin
    FOnBeforeWriteRegistro0990(strLinha);
    if strLinha <> EmptyStr then
       Add(strLinha);
  end;

  if Assigned(Registro0990) then
  begin
     with Registro0990 do
     begin
       QTD_LIN_0 := QTD_LIN_0 + 1;
       ///
       strLinha := LFill('0990') +
                   LFill(QTD_LIN_0,0);

       if Assigned(FOnWriteRegistro0990) then FOnWriteRegistro0990(strLinha);

       Add(strLinha);
     end;
  end;

  //-- After
  strLinha := '';
  if Assigned(FOnAfterWriteRegistro0990) then
  begin
    FOnAfterWriteRegistro0990(strLinha);
    if strLinha <> EmptyStr then
       Add(strLinha);
  end;
end;

end.
