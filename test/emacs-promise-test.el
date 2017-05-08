;;; -*- lexical-binding: t -*-

(ert-deftest-async
 test/promise/errors-in-resolver-are-rejections (done)
 (promise-done
  (promise-chain
      (promise-new (lambda (resolve reject)
                     (error "wut")))
    (then (lambda (result)
            (funcall done "Error did not cause promise rejection")))
    (promise-catch (lambda (exception)
                     (should (equal (error-message-string exception) "wut"))
                     (funcall done)))
    (promise-catch 'done))))

(ert-deftest-async
 test/promise/errors-in-then-are-rejections (done)
 (promise-done
  (promise-chain
      (promise-new (lambda (resolve reject)
                     (funcall resolve "yeah")))
    (then (lambda (result)
            (error "wut")))
    (then (lambda (result)
            (funcall done "Error did not cause promise rejection")))
    (promise-catch (lambda (exception)
                     (should (equal (error-message-string exception) "wut"))
                     (funcall done)))
    (promise-catch 'done))))

(ert-deftest-async
 test/promise/resolver-called-synchronously (done)
 (let ((chain-constructor-has-finished nil)
       (resolver-was-called nil))
   (promise-done
    (promise-chain
        (promise-new (lambda (resolve reject)
                       (should (equal chain-constructor-has-finished nil))
                       (setq resolver-was-called t)
                       (funcall resolve 'foo)))
      (then (lambda (result)
              (should (equal result 'foo))
              (funcall done)))
      (promise-catch (lambda (exception)
                       (funcall done exception))))
    (setq chain-constructor-has-finished t)
    (should (equal resolver-was-called t)))))
