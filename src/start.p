/*DEFINE VARIABLE hEnv AS HANDLE NO-UNDO.                           */
/*RUN quote/utils/Env.p PERSISTENT SET hEnv.                        */
/*DEFINE VARIABLE environment AS CHARACTER NO-UNDO.                 */
/*RUN GetEnvValue IN hEnv (INPUT "ENVIRONMENT", OUTPUT environment).*/
/*DISPLAY environment.                                              */