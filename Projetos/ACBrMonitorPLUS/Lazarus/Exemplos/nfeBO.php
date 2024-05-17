<?php
    /* http://www.projetoacbr.com.br/forum/topic/29588-exemplo-de-captura-de-resposta-php-socket/ */

	/* IMPORTANDO A CLASSE CLIENT SOCKET */
	require_once dirname(__FILE__).'/ClientSocket.php';

	/* MÉTODOS DE INTEGRAÇÃO COM A APLICAÇÃO ACBR AUTOMAÇÃO - NFE */

	/* MÉTODO PARA ESTABELECER UM NOVO SOCKET COM A APLICAÇÃO ACBR AUTOMAÇÃO */
	function socketBO($host, $porta) {
		try {
			$socket = new ClientSocket();
    		$socket->open($host,$porta);
    		$retorno["statusRetorno"] = "SUCESSO";
    		$retorno["socket"] = $socket;
		}
		catch (Exception $e) {
			$retorno["statusRetorno"] = "ERRO";
			$retorno["msgRetorno"] = "Não é possível estabelecer o socket solicitado.";
		}
		return $retorno;
	}

	/* MÉTODO PARA CONSULTAR O STATUS DO SERVIÇO DA NFE */
	function statusServicoNfeBO($hostAcbrMonitor, $portaAcbrMonitor) {
		try {
			/* ANALISANDO CAMPOS OBRIGATÓRIOS */
			if (empty($hostAcbrMonitor)) { throw new Exception("Falha ao identificar o host do ACBr Monitor."); }
			else if (empty($portaAcbrMonitor)) { throw new Exception("Falha ao identificar a porta do ACBr Monitor."); }

			/* ESTABELECENDO UM NOVO SOCKET */
			$socket = socketBO($hostAcbrMonitor, $portaAcbrMonitor);

			/* ANALISANDO O RETORNO DA CONEXÃO AO SOCKET */
			if ($socket["statusRetorno"] == "ERRO") { throw new Exception("Não é possível estabelecer um socket com o ACBrMonitorPlus.<br />Reinicie o computador e tente novamente. Se o problema persistir, contate o suporte técnico."); }
			else {
				/* CHAMANDO RESPOSTA DA CONEXÃO DO SOCKET */
				$socket["socket"]->recv();
				$socket["socket"]->send("NFE.StatusServico()"."\r\n.\r\n");
				$respostaFuncao = $socket["socket"]->recv();
				/* O MÉTODO PARSE_INI_STRING CONSEGUE CONVERTER A RESPOSTA DO ACBr PARA UM ARRAY.
				 * DESSA FORMA FICA MUITO FÁCIL LER A RESPOSTA. */
				$respostaFuncaoArray = parse_ini_string($respostaFuncao,true);
				if (!isset($respostaFuncaoArray["Status"]["CStat"])) {throw new Exception(nl2br($respostaFuncao)); }
				else {
					/* LIMPANDO OS DADOS DE CONEXÃO COM O SOCKET */
					$retorno["statusRetorno"] = "SUCESSO"; 
					$retorno["msgRetorno"] = $respostaFuncaoArray["Status"]["CStat"]." - ".$respostaFuncaoArray["Status"]["XMotivo"];
					return $retorno;
				}
			}
		}
		catch (Exception $e) {
			$retorno["statusRetorno"] = "ERRO";
			$retorno["msgRetorno"] = "Falha ao consultar o status do serviço da NFe.";
			$retorno["msgErro"] = $e->getMessage();
			return $retorno;
		}
	}

	/* TESTE DO MÉTODO */
	$statusServicoNfe = statusServicoNfeBO("127.0.0.1", "3434");
	print "<pre>";
	print_r($statusServicoNfe);

?>