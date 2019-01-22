{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
|* 10/08/2011: Márcio D. Carvalho
|*  - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrTEFDTicketCar;

interface

uses
  {$IfDef MSWINDOWS}
  Windows,
  {$EndIf}
  Classes, SysUtils, ACBrTEFDClass
  {$IfNDef NOGUI}
    {$If DEFINED(VisualCLX)}
      ,QControls
    {$ElseIf DEFINED(FMX)}
      ,System.UITypes
    {$ElseIf DEFINED(DELPHICOMPILER16_UP)}
      ,System.UITypes
    {$Else}
      ,Controls
    {$IfEnd}
  {$EndIf};


Const
  CACBrTEFDTicketCar_ArqTemp    = 'C:\TCS\TX\INTTCS.tmp' ;
  CACBrTEFDTicketCar_ArqReq     = 'C:\TCS\TX\INTTCS.001' ;
  CACBrTEFDTicketCar_ArqResp    = 'C:\TCS\RX\INTTCS.001' ;
  CACBrTEFDTicketCar_ArqSTS     = 'C:\TCS\RX\INTTCS.RET' ;
  CACBrTEFDCrediShop_GPExeName  = 'C:\TCS\tcs.exe' ;

type
  { TACBrTEFDRespTicketCar }

  TACBrTEFDRespTicketCar = class( TACBrTEFDResp )
  protected
    function GetTransacaoAprovada : Boolean; override;
  public
    procedure ConteudoToProperty; override;
    procedure GravaInformacao( const Identificacao : Integer;
      const Informacao : AnsiString );
  end;

  TACBrTEFDTicketCarObtemDadosVenda = procedure( var NumGNF, NumCOO, NumCNF, 
    CodProduto, IndProdServ, IndISS, PosTotalizador: Integer; var
    CodTCS, DescricaoProduto: String; var QuantProduto, VlrDesconto,
    VlrUnitario: Double ) of object ;

  {Tipos de operação}
  TACBrTEFDTicketCarObtemListaProdutos = procedure( var ListaProdutos : TStringList ) of object ;

  { TACBrTEFDTicketCar }

   TACBrTEFDTicketCar = class( TACBrTEFDClass )
   private
     fArqReq : String;
     fArqTmp : String;
     fArqResp: String;
     fArqSTS : String;
     fGPExeName : String;

     fRespostas: TStringList;

     fConteudoResp : TACBrTEFDArquivo;
     fConteudoRet  : TACBrTEFDArquivo;

     fNumLoja  : Integer;
     fNumCaixa : Integer;
     fAtualizaPrecos : Boolean;

     fDocumentosProcessados : AnsiString ;
     fOnObtemDadosVenda : TACBrTEFDTicketCarObtemDadosVenda;
     fOnObtemListaProdutos : TACBrTEFDTicketCarObtemListaProdutos;
     procedure SetArqTmp(const AValue : String);
     procedure SetArqReq(const AValue : String);
     procedure SetArqResp(const AValue : String);
     procedure SetArqSTS(const AValue : String);
     procedure SetNumLoja(const AValue : Integer);
     procedure SetNumCaixa(const AValue : Integer);
     procedure SetAtualizaPrecos(const AValue : Boolean);     

     procedure ImprimirComprovantes(imgCupom: TStringList);
     function  RealizaTransacao(operacao : String; Valor : Double): Boolean;
     function  ProcessaRespostaRequisicao(operacao: String) : Boolean;
   protected
     Function  FazerRequisicao(AHeader : AnsiString = ''; Valor : Double = 0;
       IndiceFPG_ECF : String = '') : Boolean ;
     Function ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean; override;
   public
     property Respostas : TStringList read fRespostas ;

     constructor Create(AOwner : TComponent); override;
     destructor Destroy ; override;

     procedure AtivarGP ; override;
     procedure VerificaAtivo ; override;

     procedure ConfirmarEReimprimirTransacoesPendentes ;

     Function ADM : Boolean ; override;
     Function CRT( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; override;
     Function CHQ( Valor : Double; IndiceFPG_ECF : String;
        DocumentoVinculado : String = ''; CMC7 : String = '';
        TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
        DataCheque : TDateTime = 0; Banco   : String = '';
        Agencia    : String = ''; AgenciaDC : String = '';
        Conta      : String = ''; ContaDC   : String = '';
        Cheque     : String = ''; ChequeDC  : String = '';
        Compensacao: String = '' ) : Boolean ; override;
     Procedure NCN(Rede, NSU, Finalizacao : String;
        Valor : Double = 0; DocumentoVinculado : String = ''); override;
     Procedure CNF(Rede, NSU, Finalizacao : String;
        DocumentoVinculado : String = ''); override;
     Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
        Valor : Double) : Boolean; overload; override;
   published
     property ArqTemp  : String read fArqTmp    write SetArqTmp ;
     property ArqReq   : String read fArqReq    write SetArqReq ;
     property ArqSTS   : String read fArqSTS    write SetArqSTS  ;
     property ArqResp  : String read fArqResp   write SetArqResp ;
     property GPExeName : String read fGPExeName write fGPExeName ;

     property NumLoja  : Integer read fNumLoja   write SetNumLoja ;
     property NumCaixa  : Integer read fNumCaixa  write SetNumCaixa ;
     property AtualizaPrecos : Boolean read fAtualizaPrecos write SetAtualizaPrecos ;

     property OnObtemDadosVenda : TACBrTEFDTicketCarObtemDadosVenda read fOnObtemDadosVenda write fOnObtemDadosVenda ;
     property OnObtemListaProdutos : TACBrTEFDTicketCarObtemListaProdutos read fOnObtemListaProdutos write fOnObtemListaProdutos ;
   end;

implementation

Uses ACBrUtil, dateutils, ACBrTEFD, Math, strutils;

{ TACBrTEFDRespTicketCar }

function TACBrTEFDRespTicketCar.GetTransacaoAprovada : Boolean;
begin
   Result := True ;
end;

procedure TACBrTEFDRespTicketCar.ConteudoToProperty;
var
   Linha : TACBrTEFDLinha ;
   I     : Integer;
   Parc  : TACBrTEFDRespParcela;
   LinStr: AnsiString ;
begin
   fpValorTotal := 0 ;
   fpImagemComprovante1aVia.Clear;
   fpImagemComprovante2aVia.Clear;

   for I := 0 to Conteudo.Count - 1 do
   begin
     Linha  := Conteudo.Linha[I];
     LinStr := StringToBinaryString( Linha.Informacao.AsString );

     case Linha.Identificacao of
       // TODO: Mapear mais propriedades do CliSiTef //
       100 :fpModalidadePagto              := LinStr;
       101 :fpModalidadePagtoExtenso       := LinStr;
       102 :fpModalidadePagtoDescrita      := LinStr;
       105 :
         begin
           fpDataHoraTransacaoComprovante  := Linha.Informacao.AsTimeStampSQL;
           fpDataHoraTransacaoHost         := fpDataHoraTransacaoComprovante ;
         end;
       121 : fpImagemComprovante1aVia.Text := StringToBinaryString( Linha.Informacao.AsString );
       122 : fpImagemComprovante2aVia.Text := StringToBinaryString( Linha.Informacao.AsString );
       130 :
         begin
           fpSaque      := Linha.Informacao.AsFloat ;
           fpValorTotal := fpValorTotal + fpSaque ;
         end;
       131 : fpInstituicao                 := LinStr;
       133 : fpCodigoAutorizacaoTransacao  := Linha.Informacao.AsString;
       134 : fpNSU                         := Linha.Informacao.AsString;
       //156 : fpRede                        := LinStr;
       501 : fpTipoPessoa                  := AnsiChar(IfThen(Linha.Informacao.AsInteger = 0,'J','F')[1]);
       502 : fpDocumentoPessoa             := LinStr ;
       //505 : fpQtdParcelas                 := Linha.Informacao.AsInteger;
       //506 : fpDataPreDatado               := Linha.Informacao.AsDate;

       //incluido por Evandro
       627 : fpAgencia                     := LinStr;
       628 : fpAgenciaDC                   := LinStr;
       120 : fpAutenticacao                := LinStr;
       626 : fpBanco                       := LinStr;
       613 :
        begin
          fpCheque                         := copy(LinStr, 21, 6);
          fpCMC7                           := LinStr;
        end;
       629 : fpConta                       := LinStr;
       630 : fpContaDC                     := LinStr;
       527 : fpDataVencimento              := Linha.Informacao.AsDate ; {Data Vencimento}

       //

       899 :  // Tipos de Uso Interno do ACBrTEFD
        begin
          case Linha.Sequencia of
              1 : fpCNFEnviado         := (UpperCase( Linha.Informacao.AsString ) = 'S' );
              2 : fpIndiceFPG_ECF      := Linha.Informacao.AsString ;
              3 : fpOrdemPagamento     := Linha.Informacao.AsInteger ;
            100 : fpHeader             := LinStr;
            101 : fpID                 := Linha.Informacao.AsInteger;
            102 : fpDocumentoVinculado := LinStr;
            103 : fpValorTotal         := fpValorTotal + Linha.Informacao.AsFloat;
            104 : fpRede               := Linha.Informacao.AsString ;
          end;
        end;
     end;
   end ;

   // 1 a 2 via são iguais //
   fpImagemComprovante2aVia.AddStrings( fpImagemComprovante1aVia );

   fpParcelas.Clear;
   for I := 1 to fpQtdParcelas do
   begin
      Parc := TACBrTEFDRespParcela.create;
      Parc.Vencimento := LeInformacao( 141, I).AsDate ;
      Parc.Valor      := LeInformacao( 142, I).AsFloat ;

      fpParcelas.Add(Parc);
   end;
end;

procedure TACBrTEFDRespTicketCar.GravaInformacao(const Identificacao : Integer;
   const Informacao : AnsiString);
begin
  fpConteudo.GravaInformacao( Identificacao, 0,
                              BinaryStringToString(Informacao) ); // Converte #10 para "\x0A"
end;

{ TACBrTEFDClass }

constructor TACBrTEFDTicketCar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ArqReq    := CACBrTEFDTicketCar_ArqReq ;
  ArqResp   := CACBrTEFDTicketCar_ArqResp ;
  ArqTemp   := CACBrTEFDTicketCar_ArqTemp ;
  ArqSTS    := CACBrTEFDTicketCar_ArqSTS ;
  GPExeName := CACBrTEFDCrediShop_GPExeName;
  fpTipo    := gpTicketCar;

  fConteudoRet  := TACBrTEFDArquivo.Create;
  fConteudoResp := TACBrTEFDArquivo.Create;

  Name      := 'TicketCar' ;
end;

destructor TACBrTEFDTicketCar.Destroy;
begin
   fRespostas.Free ;
   fConteudoRet.Free;
   fConteudoResp.Free;

   inherited Destroy;
end;

procedure TACBrTEFDTicketCar.SetArqTmp(const AValue : String);
begin
  fArqTmp := Trim( AValue ) ;
end;

procedure TACBrTEFDTicketCar.SetArqReq(const AValue : String);
begin
  fArqReq := Trim( AValue ) ;
end;

procedure TACBrTEFDTicketCar.SetArqResp(const AValue : String);
begin
  fArqResp := Trim( AValue ) ;
end;

procedure TACBrTEFDTicketCar.SetArqSTS(const AValue : String);
begin
  fArqSTS := Trim( AValue ) ;
end;

procedure TACBrTEFDTicketCar.SetNumLoja(const AValue : Integer);
begin
  fNumLoja := AValue ;
end;

procedure TACBrTEFDTicketCar.SetNumCaixa(const AValue : Integer);
begin
  fNumCaixa := AValue ;
end;

procedure TACBrTEFDTicketCar.SetAtualizaPrecos(const AValue : Boolean);
begin
  fAtualizaPrecos := AValue ;
end;

procedure TACBrTEFDTicketCar.AtivarGP;
begin
   raise EACBrTEFDErro.Create( ACBrStr( 'CliDTEF não pode ser ativado localmente' )) ;
end;

procedure TACBrTEFDTicketCar.VerificaAtivo;
begin
   {Nada a Fazer}
end;

procedure TACBrTEFDTicketCar.ConfirmarEReimprimirTransacoesPendentes;
Var
  ArquivosVerficar : TStringList ;
  ArqMask, NSUs    : AnsiString;
  ExibeMsg         : Boolean ;
begin
  ArquivosVerficar := TStringList.Create;

  try
     ArquivosVerficar.Clear;

     { Achando Arquivos de Backup deste GP }
     ArqMask := TACBrTEFD(Owner).PathBackup + PathDelim + 'ACBr_' + Self.Name + '_*.tef' ;
     FindFiles( ArqMask, ArquivosVerficar, True );
     NSUs := '' ;
     ExibeMsg := (ArquivosVerficar.Count > 0) ;

     { Enviando NCN ou CNC para todos os arquivos encontrados }
     while ArquivosVerficar.Count > 0 do
     begin
        if not FileExists( ArquivosVerficar[ 0 ] ) then
        begin
           ArquivosVerficar.Delete( 0 );
           Continue;
        end;

        Resp.LeArquivo( ArquivosVerficar[ 0 ] );

        try
           if pos(Resp.DocumentoVinculado, fDocumentosProcessados) = 0 then
              CNF(Resp.Rede, Resp.NSU, '', '');   {Confirma}

           if Resp.NSU <> '' then
              NSUs := NSUs + sLineBreak + 'NSU: '+Resp.NSU ;

           DeleteFile( ArquivosVerficar[ 0 ] );
           ArquivosVerficar.Delete( 0 );
        except
        end;
     end;

     if ExibeMsg then
        TACBrTEFD(Owner).DoExibeMsg( opmOK,
                               'Transação TEF efetuada.'+sLineBreak+
                               'Favor Re-Imprimir Ultimo Cupom ' + NSUs ) ;

  finally
     ArquivosVerficar.Free;
  end;
end;

Function TACBrTEFDTicketCar.ADM : Boolean;
begin
  Result := FazerRequisicao('ADM', 0, '0' );
end;

Function TACBrTEFDTicketCar.CRT( Valor : Double; IndiceFPG_ECF : String;
   DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean;
begin
  Result := False;

  VerificarTransacaoPagamento( Valor );

  if FazerRequisicao('CRT', Valor, DocumentoVinculado ) then
    begin
      ProcessarRespostaPagamento( IndiceFPG_ECF, Valor );
      Result := True;
    end;
end;

Function TACBrTEFDTicketCar.CHQ(Valor : Double; IndiceFPG_ECF : String;
   DocumentoVinculado : String; CMC7 : String; TipoPessoa : AnsiChar;
   DocumentoPessoa : String; DataCheque : TDateTime; Banco : String;
   Agencia : String; AgenciaDC : String; Conta : String; ContaDC : String;
   Cheque : String; ChequeDC : String; Compensacao: String) : Boolean ;
begin
  Result := False;
end;

Procedure TACBrTEFDTicketCar.CNF(Rede, NSU, Finalizacao : String;
   DocumentoVinculado : String) ;
begin
  //
end;

Function TACBrTEFDTicketCar.CNC(Rede, NSU : String;
   DataHoraTransacao : TDateTime; Valor : Double) : Boolean;
begin
  Result := True;
end;

Procedure TACBrTEFDTicketCar.NCN(Rede, NSU, Finalizacao : String;
   Valor : Double; DocumentoVinculado : String) ;
begin
  //
end;

Function TACBrTEFDTicketCar.FazerRequisicao(AHeader : AnsiString = '';
  Valor : Double = 0; IndiceFPG_ECF : String = '') : Boolean ;
Var
  Erro, aNSU : AnsiString;
  ArquivoResposta : TStringList;
  i : integer;
begin
  Result   := False ;

  ApagaEVerifica( ArqTemp );  // Apagando Arquivo Temporario anterior //
  ApagaEVerifica( ArqReq );   // Apagando Arquivo de Requisicao anterior //
  ApagaEVerifica( ArqSTS );   // Apagando Arquivo de Retorno da resposta //
  ApagaEVerifica( ArqResp );  // Apagando Arquivo de Resposta anterior //

  if fpAguardandoResposta then
    raise EACBrTEFDErro.Create( ACBrStr( 'Requisição anterior não concluida' ) ) ;

  aNSU := FormatDateTime('HHNNSS', Now );;

  if AHeader = 'CRT' then
    begin
      Result := RealizaTransacao('CRT', Valor);
    end;

  if (AHeader = 'ADM') then
    begin
      Result := RealizaTransacao('ADM', 0);
    end;

  Erro := '';

  if not Result then
     raise EACBrTEFDErro.Create( ACBrStr( Erro ) )
  else
    begin
      Resp.Clear;

      with TACBrTEFDRespTicketCar( Resp ) do
      begin
        ArquivoResposta := TStringList.Create;
        try

           for i := 1 to fConteudoResp.LeInformacao(514, 0).AsInteger do
             ArquivoResposta.Add(copy(fConteudoResp.LeInformacao(515, i).AsString, 2 , Length(fConteudoResp.LeInformacao(515, i).AsString)-2));

           if (AHeader = 'ADM') then
             begin
               ImprimirComprovantes(ArquivoResposta);
             end;
           if (AHeader = 'CRT') then
             begin
               fConteudoResp.GravaInformacao(134,000, aNSU);
               fConteudoResp.GravaInformacao(102,000, 'T.E.F.');
               fConteudoResp.GravaInformacao(121,000, BinaryStringToString(ArquivoResposta.Text));

               Conteudo.Conteudo.Text := fConteudoResp.Conteudo.Text;
               Conteudo.GravaInformacao(899,100, AHeader ) ;
               Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
               Conteudo.GravaInformacao(899,102, IndiceFPG_ECF ) ;
               Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );
               Conteudo.GravaInformacao(899,104, AHeader );

               Resp.TipoGP := fpTipo;
             end;
        finally
           ArquivoResposta.Free;
        end ;
      end;
    end;
end;

function TACBrTEFDTicketCar.ProcessaRespostaRequisicao(operacao: String) : Boolean;
var
   TempoInicioEspera : Double;
   Interromper, OK   : Boolean;
begin
  Result := False;
  Interromper := False ;
  OK          := False ;
  try
     while not (OK or Interromper) do
     begin
        TempoInicioEspera := now ;
        fpAguardandoResposta := True ;

        try
           repeat
              Sleep( TACBrTEFD(Owner).EsperaSleep );  // Necessário Para não sobrecarregar a CPU //

              with TACBrTEFD(Owner) do
              begin
                 if Assigned( OnAguardaResp ) then
                    OnAguardaResp( ArqSTS, SecondsBetween(TempoInicioEspera, Now),
                                   Interromper ) ;
              end ;
           until FileExists( ArqSTS ) or Interromper ;
        finally
           fpAguardandoResposta := False ;
           with TACBrTEFD(Owner) do
           begin
              if Assigned( OnAguardaResp ) then
                 OnAguardaResp( ArqSTS, -1, Interromper ) ;
           end ;
        end;

        fConteudoRet.LeArquivo( ArqSTS );

        if fConteudoRet.LeInformacao(517, 0).AsInteger <> 0 then
          begin
             DeleteFile(ArqSTS);
             TACBrTEFD(Owner).DoExibeMsg(opmOK, fConteudoRet.LeInformacao(517, 1).AsString);
             fConteudoRet.Clear;
             Interromper := True;
             Exit;
          end;

        if FileExists( ArqResp ) then
          begin
            fConteudoResp.LeArquivo( ArqResp );
            DeleteFile(ArqSTS);
            DeleteFile(ArqResp);
            
            //if ((operacao = operCRT) or (operacao = operCancelamento))  then
              //CNF('', '', '');

            OK := True;
            Result := True;            
          end
        else
          begin
             fConteudoRet.Clear;
             Interromper := True;
             Exit;
          end;
     end ;
  finally
    DeleteFile( ArqReq );
  end ;
end;

function TACBrTEFDTicketCar.ProcessarRespostaPagamento( const IndiceFPG_ECF : String;
        const Valor : Double) : Boolean;
var
  ImpressaoOk : Boolean;
  RespostaPendente : TACBrTEFDResp ;
begin
  Result := True ;

  with TACBrTEFD(Owner) do
  begin
     Self.Resp.IndiceFPG_ECF := IndiceFPG_ECF;

     { Cria Arquivo de Backup, contendo Todas as Respostas }
     CopiarResposta ;

     { Cria cópia do Objeto Resp, e salva no ObjectList "RespostasPendentes" }
     RespostaPendente := TACBrTEFDRespTicketCar.Create ;
     RespostaPendente.Assign( Resp );
     RespostasPendentes.Add( RespostaPendente );

     if AutoEfetuarPagamento then
     begin
        ImpressaoOk := False ;

        try
           while not ImpressaoOk do
           begin
              try
                 ECFPagamento( IndiceFPG_ECF, Valor );
                 RespostasPendentes.SaldoAPagar  := RoundTo( RespostasPendentes.SaldoAPagar - Valor, -2 ) ;
                 RespostaPendente.OrdemPagamento := RespostasPendentes.Count + 1 ;
                 ImpressaoOk := True ;
              except
                 on EACBrTEFDECF do ImpressaoOk := False ;
                 else
                    raise ;
              end;

              if not ImpressaoOk then
              begin
                 if DoExibeMsg( opmYesNo, 'Impressora não responde'+sLineBreak+
                                          'Deseja imprimir novamente ?') <> mrYes then
                 begin
                    try ComandarECF(opeCancelaCupom); except {Exceção Muda} end ;
                    break ;
                 end;
              end;
           end;
        finally
           if not ImpressaoOk then
              CancelarTransacoesPendentes;
        end;
     end;

     if RespostasPendentes.SaldoRestante <= 0 then
     begin
        if AutoFinalizarCupom then
        begin
           FinalizarCupom( False );  { False não desbloqueia o MouseTeclado }
           ImprimirTransacoesPendentes;
        end;
     end ;
  end;
end;

procedure TACBrTEFDTicketCar.ImprimirComprovantes(imgCupom: TStringList);
var
  ImpressaoOk, FechaGerencialAberto, GerencialAberto : Boolean;
  Est : AnsiChar;
begin
  CopiarResposta;

  GerencialAberto      := False;
  ImpressaoOk          := False ;
  FechaGerencialAberto := False;

  with TACBrTEFD(Owner) do
  begin
    try
      while not ImpressaoOk do
      begin
        try
           if FechaGerencialAberto then
           begin
             Est := EstadoECF;

             { Fecha Vinculado ou Gerencial ou Cupom, se ficou algum aberto por Desligamento }
             case Est of
               'C'      : ComandarECF( opeFechaVinculado );
               'G', 'R' : ComandarECF( opeFechaGerencial );
               'V', 'P' : ComandarECF( opeCancelaCupom );
             end;

             GerencialAberto      := False ;
             FechaGerencialAberto := False ;

             if EstadoECF <> 'L' then
               raise EACBrTEFDECF.Create( ACBrStr('ECF não está LIVRE') ) ;
           end;

            if imgCupom.Text <> '' then
            begin
              if not GerencialAberto then
               begin
                 ComandarECF( opeAbreGerencial ) ;
                 GerencialAberto := True ;
               end;

              ECFImprimeVia( trGerencial, 1, imgCupom );

              ImpressaoOk := True ;
            end;

          if GerencialAberto then
          begin
             ComandarECF( opeFechaGerencial );
             GerencialAberto := False;
          end;
        except
          on EACBrTEFDECF do ImpressaoOk := False ;
          else
             raise ;
        end;

        if not ImpressaoOk then
        begin
          if DoExibeMsg( opmYesNo, 'Impressora não responde'+sLineBreak+
                                   'Deseja imprimir novamente ?') <> mrYes then
            break ;

          FechaGerencialAberto := True ;
        end;
      end ;
    finally
      //
    end;
  end;
end;

function TACBrTEFDTicketCar.RealizaTransacao(operacao : String; Valor : Double): Boolean;
var ArquivoRequisicao : TStringList;
    NumGNF, NumCOO,
    NumCNF, CodProduto, IndProdServ, IndISS,
    PosTotalizador : Integer;
    CodTCS, DescricaoProduto: String;
    QuantProduto, VlrDesconto, VlrUnitario : Double;
begin
  ArquivoRequisicao := TStringList.Create;
  try
     if operacao = 'ADM' then
       begin
         if fAtualizaPrecos then
           begin
             fOnObtemListaProdutos(ArquivoRequisicao);
           end
         else
           begin
             ArquivoRequisicao.Add('500-000 = ADM');
             ArquivoRequisicao.Add('501-000 = ' + IntToStr(SecondOfTheDay(now)));
             ArquivoRequisicao.Add('999-999 = 0')
           end;
       end;

     if operacao = 'CRT' then
       begin
         NumGNF           := 0;
         NumCOO           := 0;
         NumCNF           := 0;
         CodProduto       := 0;
         IndProdServ      := 0;
         IndISS           := 0;
         PosTotalizador   := 0;
         CodTCS           := '';
         DescricaoProduto := '';
         QuantProduto     := 0;
         VlrDesconto      := 0;
         VlrUnitario      := 0;
         fOnObtemDadosVenda(NumGNF, NumCOO, NumCNF, CodProduto,
                            IndProdServ, IndISS, PosTotalizador,
                            CodTCS, DescricaoProduto, QuantProduto, VlrDesconto,
                            VlrUnitario);

         ArquivoRequisicao.Add('500-000 = CRT');
         ArquivoRequisicao.Add('501-000 = ' + IntToStr(SecondOfTheDay(now)));
         ArquivoRequisicao.Add('502-000 = ' + IntToStr(fNumCaixa));
         ArquivoRequisicao.Add('503-000 = ' + IntToStr(NumGNF));
         ArquivoRequisicao.Add('504-000 = ' + IntToStr(NumCOO));
         ArquivoRequisicao.Add('505-000 = ' + IntToStr(fNumLoja));
         ArquivoRequisicao.Add('506-000 = ' + CodTCS);
         ArquivoRequisicao.Add('507-000 = ' + FormatFloat('0', QuantProduto * 100));
         ArquivoRequisicao.Add('508-000 = ' + FormatFloat('0', VlrDesconto * 100));
         ArquivoRequisicao.Add('509-000 = ' + IntToStr(CodProduto));
         ArquivoRequisicao.Add('509-001 = ' + DescricaoProduto);
         ArquivoRequisicao.Add('510-000 = ' + FormatFloat('0', Valor * 100));
         ArquivoRequisicao.Add('518-000 = ' + IntToStr(IndProdServ));
         ArquivoRequisicao.Add('519-000 = ' + FormatFloat('0', VlrUnitario * 1000));
         ArquivoRequisicao.Add('520-000 = ' + IntToStr(IndISS));
         ArquivoRequisicao.Add('521-000 = ' + IntToStr(NumCNF));
         ArquivoRequisicao.Add('522-000 = ' + IntToStr(PosTotalizador));
         ArquivoRequisicao.Add('999-999 = 0');
       end;

     ArquivoRequisicao.SaveToFile(CACBrTEFDTicketCar_ArqTemp);
  finally
     ArquivoRequisicao.Free;
  end ;

  RenameFile(fArqTmp, fArqReq);

  if fAtualizaPrecos then
    RunCommand( GPExeName, '/A' )
  else
    RunCommand( GPExeName );

  Result := ProcessaRespostaRequisicao(operacao);
end;

end.

