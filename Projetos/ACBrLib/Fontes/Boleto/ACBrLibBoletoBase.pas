{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: José M. S. Junior                               }
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

{$I ACBr.inc}

unit ACBrLibBoletoBase;


interface

uses
  Classes, SysUtils, Forms, ACBrLibComum, ACBrLibBoletoDataModule;

type

  {TACBrLibBoleto}

  TACBrLibBoleto = class(TACBrLib)
  private
    FBoletoDM: TLibBoletoDM;

    function ListaBancos(): AnsiString;
    function ListaCaractTitulo() : AnsiString;
    function ListaOcorrencias(): AnsiString;
    function ListaOcorrenciasEX(): AnsiString;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property BoletoDM: TLibBoletoDM read FBoletoDM;

    function ConfigurarDados(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function IncluirTitulos(eArquivoIni, eTpSaida: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function LimparLista: longint;
    function TotalTitulosLista(const sResposta: PChar; var esTamanho: longint): longint;
    function Imprimir(eNomeImpressora: PChar): longint;
    function ImprimirBoleto(eIndice: longint; eNomeImpressora: PChar): longint;
    function GerarPDF: longint;
    function GerarHTML: longint;
    function GerarRemessa(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
    function LerRetorno(eDir, eNomeArq: PChar): longint;
    function EnviarEmail(ePara, eAssunto, eMensagem, eCC: PChar): longint;
    function SetDiretorioArquivo(eDir, eArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ListaBancos(const sResposta: PChar; var esTamanho: longint): longint;
    function ListaCaractTitulo(const sResposta: PChar; var esTamanho: longint): longint;
    function ListaOcorrencias(const sResposta: PChar; var esTamanho: longint): longint;
    function ListaOcorrenciasEX(const sResposta: PChar; var esTamanho: longint): longint;
    function TamNossoNumero(eCarteira, enossoNumero, eConvenio: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function CodigosMoraAceitos(const sResposta: PChar; var esTamanho: longint): longint;
    function SelecionaBanco(eCodBanco: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function MontarNossoNumero(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function RetornaLinhaDigitavel(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function RetornaCodigoBarras(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;

  end;

implementation

uses
  ACBrLibConsts, ACBrLibBoletoConsts, ACBrLibConfig, ACBrUtil, strutils, typinfo,
  ACBrBoleto, ACBrBoletoConversao, ACBrLibBoletoConfig, ACBrMail;
  
constructor TACBrLibBoleto.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited
  Create(ArqConfig, ChaveCrypt);
  FBoletoDM := TLibBoletoDM.Create(nil);
  FBoletoDM.Lib := Self;
end;

destructor TACBrLibBoleto.Destroy;
begin
  FBoletoDM.Free;
  inherited Destroy;
end;
  
procedure TACBrLibBoleto.Inicializar;
begin
  GravarLog('TACBrLibBoleto.Inicializar', logNormal);

  FBoletoDM.CriarACBrMail;

  GravarLog('TACBrLibBoleto.Inicializar - Feito', logParanoico);

  inherited Inicializar;
end;

procedure TACBrLibBoleto.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibBoletoConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibBoleto.Executar;
begin
  inherited Executar;
  FBoletoDM.AplicarConfiguracoes;
end;

function TACBrLibBoleto.ConfigurarDados(eArquivoIni: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;  
var
  Resposta : AnsiString;
  ArquivoIni : String;
begin
  try
    ArquivoIni := String(eArquivoIni);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_ConfigurarDados(' + ArquivoIni + ' )', logCompleto, True)
    else
      GravarLog('Boleto_ConfigurarDados', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      if not (BoletoDM.ACBrBoleto1.LerArqIni( ArquivoIni )) then
        Resposta := Format( SErroLerArquivoEntrada, [ArquivoIni])
      else
        Resposta := 'OK';

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.IncluirTitulos(eArquivoIni, eTpSaida: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;  
var
  Resposta : AnsiString;
  ArquivoIni : String;
  TpSaida : String;
  Mensagem : TStringList;
begin
  try
    ArquivoIni := String(eArquivoIni);
    TpSaida := String(eTpSaida);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_IncluirTitulos(' + ArquivoIni + ' )', logCompleto, True)
    else
      GravarLog('Boleto_IncluirTitulos', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      if not (BoletoDM.ACBrBoleto1.LerArqIni( ArquivoIni )) then
        Resposta := Format( SErroLerArquivoEntrada, [ArquivoIni])
      else
      begin
        if TpSaida = 'I' then
          BoletoDM.ACBrBoleto1.Imprimir
        else if TpSaida = 'P' then
          BoletoDM.ACBrBoleto1.GerarPDF
        else if TpSaida = 'E' then
        begin
          with TLibBoletoConfig(Config).BoletoConfig do
          begin
            Mensagem := TStringList.Create;
            try
              Mensagem.Add(emailMensagemBoleto);
              if Config.Log.Nivel > logNormal then
                GravarLog('Boleto_EnviarEmail(' + BoletoDM.ACBrBoleto1.ListadeBoletos[0].Sacado.Email +
                          ',' + emailAssuntoBoleto + ',' + Mensagem.Text, logCompleto, True)
              else
                GravarLog('Boleto_EnviarEmail', logNormal);

              BoletoDM.ACBrBoleto1.EnviarEmail( BoletoDM.ACBrBoleto1.ListadeBoletos[0].Sacado.Email,
                                               emailAssuntoBoleto,
                                               Mensagem,
                                               True );
              Result := SetRetorno(ErrOK);
            finally
              Mensagem.Free;
            end;
          end;
        end;
        Resposta := 'OK';
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.LimparLista: longint;  
begin
  try
    GravarLog('Boleto_LimparLista', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ACBrBoleto1.ListadeBoletos.Clear;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.TotalTitulosLista(const sResposta: PChar; var esTamanho: longint): longint;  
var
  Resposta: AnsiString;
begin
  try
    GravarLog('Boleto_TotalTitulosLista', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := IntToStr( BoletoDM.ACBrBoleto1.ListadeBoletos.Count );

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.Imprimir(eNomeImpressora: PChar): longint;  
var
  NomeImpressora : String;
begin
  try
    NomeImpressora := String(eNomeImpressora);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_Imprimir(' + NomeImpressora + ' )', logCompleto, True)
    else
      GravarLog('Boleto_Imprimir', logNormal);

    BoletoDM.Travar;
    try
      if NaoEstaVazio(NomeImpressora) then
        BoletoDM.ACBrBoleto1.ACBrBoletoFC.PrinterName := NomeImpressora;

      BoletoDM.ACBrBoleto1.Imprimir;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.ImprimirBoleto(eIndice: longint; eNomeImpressora: PChar): longint;  
var
  NomeImpressora : String;
begin
  try
    NomeImpressora := String(eNomeImpressora);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_ImprimirBoleto(' + IntToStr(eIndice) + ', ' + NomeImpressora + ' )', logCompleto, True)
    else
      GravarLog('Boleto_ImprimirBoleto', logNormal);

    BoletoDM.Travar;
    try
      if NaoEstaVazio(NomeImpressora) then
        BoletoDM.ACBrBoleto1.ACBrBoletoFC.PrinterName := NomeImpressora;

      BoletoDM.ACBrBoleto1.ACBrBoletoFC.IndiceImprimirIndividual := eIndice;
      try
        BoletoDM.ACBrBoleto1.Imprimir;
      finally
        BoletoDM.ACBrBoleto1.ACBrBoletoFC.IndiceImprimirIndividual := -1;
      end;

      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.GerarPDF: longint;  
begin
  try
    GravarLog('Boleto_GerarPDF', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ACBrBoleto1.GerarPDF;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.GerarHTML: longint;  
begin
  try
    GravarLog('Boleto_GerarHTML', logNormal);

    BoletoDM.Travar;
    try
      BoletoDM.ACBrBoleto1.GerarHTML;
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.GerarRemessa(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;  
var
  Dir: String;
  NumArquivo: Integer;
  NomeArq: String;
begin
  try
    Dir := String(eDir);
    NumArquivo:= StrToIntDef( IntToStr( eNumArquivo ), 0);
    NomeArq:= String(eNomeArq);
    GravarLog('Boleto_GerarRemessa', logNormal);

    BoletoDM.Travar;
    try
      if NaoEstaVazio( Dir ) then
        BoletoDM.ACBrBoleto1.DirArqRemessa := Dir;
      if NaoEstaVazio( NomeArq ) then
        BoletoDM.ACBrBoleto1.NomeArqRemessa:= NomeArq;

      BoletoDM.ACBrBoleto1.GerarRemessa( NumArquivo );
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.LerRetorno(eDir, eNomeArq: PChar): longint;  
var
  Dir: String;
  NomeArq: String;
begin
  try
    Dir := String(eDir);
    NomeArq:= String(eNomeArq);
    GravarLog('Boleto_LerRetorno', logNormal);

    BoletoDM.Travar;
    try
      if NaoEstaVazio( Dir ) then
        BoletoDM.ACBrBoleto1.DirArqRetorno := Dir;
      if NaoEstaVazio( NomeArq ) then
        BoletoDM.ACBrBoleto1.NomeArqRetorno:= NomeArq;

      BoletoDM.ACBrBoleto1.LerRetorno();
      BoletoDM.ACBrBoleto1.GravarArqIni(Dir,'');
      Result := SetRetorno(ErrOK);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.EnviarEmail(ePara, eAssunto, eMensagem, eCC: PChar): longint;  
var
  Para, Assunto, Mensagem, CC: String;
  slMensagem, slCC: TStrings;
begin
  try
    Para := String(ePara);
    Assunto := String(eAssunto);
    Mensagem := String(eMensagem);
    CC := String(eCC);

    if Config.Log.Nivel > logNormal then
      GravarLog('Boleto_EnviarEmail(' + Para + ',' + Assunto
      + ',' + Mensagem + ',' + CC +')', logCompleto, True)
    else
      GravarLog('Boleto_EnviarEmail', logNormal);

    BoletoDM.Travar;

    if EstaVazio(ePara) and (BoletoDM.ACBrBoleto1.ListadeBoletos.Count > 0) then
      Para := BoletoDM.ACBrBoleto1.ListadeBoletos[0].Sacado.Email;

    try
      slMensagem := TStringList.Create;
      slMensagem.Text := Mensagem;

      slCC := TStringList.Create;
      slCC.Text := CC;

      BoletoDM.ACBrBoleto1.EnviarEmail(Para, Assunto, slMensagem, True, slCC);
      Result := SetRetorno(ErrOK);
    finally
      slMensagem.Free;
      slCC.Free;
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.SetDiretorioArquivo(eDir, eArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;  
var
  Resposta : AnsiString;
  Dir : String;
  Arq : String;
begin
   try
     Dir := String(eDir);
     Arq := String(eArq);

     if Config.Log.Nivel > logNormal then
       GravarLog('Boleto_SetDiretorioArquivo( Diretorio: ' + Dir +
       ' Arquivo: ' + Arq + ' )', logCompleto, True)
     else
       GravarLog('Boleto_SetDiretorioArquivo', logNormal);

     if DirectoryExists(Dir) then
     begin
       BoletoDM.Travar;
       try
         Resposta := '';
         if BoletoDM.ACBrBoleto1.ACBrBoletoFC.Filtro = TACBrBoletoFCFiltro(fiHTML) then
           BoletoDM.ACBrBoleto1.ACBrBoletoFC.NomeArquivo := PathWithDelim( Dir )  +
           IfThen(NaoEstaVazio(Arq), Arq , 'boleto.html' )
         else
           BoletoDM.ACBrBoleto1.ACBrBoletoFC.NomeArquivo := PathWithDelim( Dir ) +
           IfThen(NaoEstaVazio(Arq), Arq , 'boleto.pdf' );

         Resposta := BoletoDM.ACBrBoleto1.ACBrBoletoFC.NomeArquivo;
         MoverStringParaPChar(Resposta, sResposta, esTamanho);
         Result := SetRetorno(ErrOK, Resposta);
       finally
         BoletoDM.Destravar;
       end;
     end
     else
       Result := SetRetorno(ErrDiretorioNaoExiste);
   except
     on E: EACBrLibException do
       Result := SetRetorno(E.Erro, E.Message);
     on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, E.Message);
   end;
end;

function TACBrLibBoleto.ListaBancos(const sResposta: PChar; var esTamanho: longint): longint;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_SetDiretorioArquivo', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaBancos;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.ListaCaractTitulo(const sResposta: PChar; var esTamanho: longint): longint;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_ListaCaractTitulo', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaCaractTitulo();
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.ListaOcorrencias(const sResposta: PChar; var esTamanho: longint): longint;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_ListaOcorrencias', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaOcorrencias();
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.ListaOcorrenciasEX(const sResposta: PChar; var esTamanho: longint): longint;  
var
   Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_ListaOcorrenciasEX', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := ListaOcorrenciasEX();
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.TamNossoNumero(eCarteira, enossoNumero, eConvenio: PChar; const sResposta: PChar; var esTamanho: longint): longint;  
var
   Resposta : AnsiString;
   Carteira, NossoNumero, Convenio : String;
begin
  try
    Carteira := String(eCarteira);
    NossoNumero:= String(enossoNumero);
    Convenio:= String(eConvenio);

    GravarLog('Boleto_TamNossoNumero', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := IntToStr(BoletoDM.ACBrBoleto1.Banco.CalcularTamMaximoNossoNumero(Carteira, NossoNumero, Convenio));
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.CodigosMoraAceitos(const sResposta: PChar; var esTamanho: longint): longint;  
var
  Resposta : AnsiString;
begin
  try
    GravarLog('Boleto_CodigosMoraAceitos', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      Resposta := BoletoDM.ACBrBoleto1.Banco.CodigosMoraAceitos;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.SelecionaBanco(eCodBanco: PChar; const sResposta: PChar; var esTamanho: longint): longint;  
var
   Resposta : AnsiString;
   CodBanco : String;
begin
  try
    CodBanco := String(eCodBanco);

    GravarLog('Boleto_SelecionaBanco', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      BoletoDM.ACBrBoleto1.Banco.TipoCobranca := BoletoDM.ACBrBoleto1.GetTipoCobranca(StrToInt64Def(Trim(CodBanco),0));

      Resposta := CodBanco;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.MontarNossoNumero(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
   Resposta : AnsiString;
   Indice : Integer;
begin
  try
    Indice := StrToInt( IntToStrZero( eIndice,1 ));
    GravarLog('Boleto_MontarNossoNumero', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      if (BoletoDM.ACBrBoleto1.ListadeBoletos.Count = 0) then
        Result := SetRetorno(ErrArquivoNaoExiste)
      else
      begin
        Resposta := BoletoDM.ACBrBoleto1.Banco.MontarCampoNossoNumero(
                    BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.RetornaLinhaDigitavel(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
   Resposta : AnsiString;
   Indice : Integer;
   ABarras : String;
begin
  try
    Indice := StrToInt( IntToStrZero( eIndice, 1 ));
    GravarLog('RetornaLinhaDigitavel', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      if (BoletoDM.ACBrBoleto1.ListadeBoletos.Count = 0) then
        Result := SetRetorno(ErrArquivoNaoExiste)
      else
      begin
        ABarras := BoletoDM.ACBrBoleto1.Banco.MontarCodigoBarras(BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
        Resposta := BoletoDM.ACBrBoleto1.Banco.MontarLinhaDigitavel(ABarras, BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.RetornaCodigoBarras(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
   Resposta : AnsiString;
   Indice : Integer;
begin
  try
    Indice := StrToInt( IntToStrZero( eIndice,1 ));
    GravarLog('Boleto_RetornaCodigoBarras', logNormal);

    BoletoDM.Travar;
    try
      Resposta := '';
      if (BoletoDM.ACBrBoleto1.ListadeBoletos.Count = 0) then
        Result := SetRetorno(ErrArquivoNaoExiste)
      else
      begin
        Resposta := BoletoDM.ACBrBoleto1.Banco.MontarCodigoBarras(BoletoDM.ACBrBoleto1.ListadeBoletos[Indice]);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      BoletoDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibBoleto.ListaBancos: AnsiString;
var
   SBanco : String;
   I: Integer;
begin
   Result := '';
   for i:= integer( Low(TACBrTipoCobranca) ) + 1  to integer( High(TACBrTipoCobranca) ) do
   begin
     sBanco := GetEnumName( TypeInfo(TACBrTipoCobranca), Integer(I) );
     sBanco := copy(SBanco,4, Length(SBanco)); // Removendo "cob" do nome do banco.
     Result := Result + sBanco + '|';
   end;

   if Result <> '' then
      Result := copy(Result,1,Length(Result)-1) ;
end;

function TACBrLibBoleto.ListaCaractTitulo: AnsiString;
var
   ICaractTitulo : TACBrCaracTitulo;
   SCaractTitulo : String;
begin

   for ICaractTitulo := Low(TACBrCaracTitulo) to high(TACBrCaracTitulo) do
   begin
     SCaractTitulo := GetEnumName( TypeInfo(TACBrCaracTitulo), Integer(ICaractTitulo) );
     SCaractTitulo := copy(SCaractTitulo, 3, Length(SCaractTitulo)); // Removendo "tc".
     Result := Result + SCaractTitulo + '|';
   end;

   if Result <> '' then
      Result := copy(Result,1,Length(Result)-1) ;
end;

function TACBrLibBoleto.ListaOcorrencias: AnsiString;
var
   ITipoOcorrencia : TACBrTipoOcorrencia;
   SOcorrencia     : String;
begin
  for ITipoOcorrencia := Low(TACBrTipoOcorrencia) to High(TACBrTipoOcorrencia) do
  begin
    SOcorrencia := GetEnumName( TypeInfo(TACBrTipoOcorrencia), Integer(ITipoOcorrencia) ) ;
    Result := Result + copy(SOcorrencia, 3, Length(SOcorrencia)) + '|';  //Remove "to"
  end;

  if (Result <> '') then
    Result := copy(Result,1,Length(Result)-1) ;
end;

function TACBrLibBoleto.ListaOcorrenciasEX: AnsiString;
var
   ITipoOcorrencia : TACBrTipoOcorrencia;
   SOcorrencia     : String;
   ValorIndice     : Integer;
begin

  for ITipoOcorrencia := Low(TACBrTipoOcorrencia) to High(TACBrTipoOcorrencia) do
  begin
    ValorIndice := Integer(ITipoOcorrencia);
    SOcorrencia := GetEnumName( TypeInfo(TACBrTipoOcorrencia), ValorIndice ) ;
    Result := Result + IntToStr(ValorIndice) + '-' +
              copy(SOcorrencia, 3, Length(SOcorrencia)) + '|';  //Remove "to"
  end;

  if (Result <> '') then
    Result := copy(Result, 1, Length(Result)-1) ;
end;

end.

