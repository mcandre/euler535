guard :shell do
  watch(/^*\.hs$/) do |m|
    title = 'test'
    eager "runhaskell #{m[0]}"
    status = ($CHILD_STATUS.success? && :success) || :failed
    n '', title, status
    ''
  end
end